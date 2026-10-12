#!/usr/bin/env bash
# Build the repo the orchestrator clip is filmed in.
#
# One service, checked out once. The clip's setup plugin cuts a worktree per
# task off it, which is the thing being filmed — so what this has to provide is
# a real git repo with enough source in it that a code pane beside an agent
# reads as code rather than as an empty buffer.
#
# No `Cargo.toml`, deliberately: a project manifest is an executable-content
# marker, which opens the workspace Restricted, which blocks the very
# `spawnProcess` calls the orchestrator makes to run git and the agents.
set -euo pipefail

# The directory name is what the dock calls the launch workspace, so the
# checkout is `api` rather than the name of the clip that films it.
ROOT="${1:-$HOME/repos/fresh/target/clips/fresh-orchestrator-dock/api}"
rm -rf "$(dirname "$ROOT")"
mkdir -p "$ROOT/src/db" "$ROOT/tests"

cat > "$ROOT/README.md" <<'EOF'
# api

The public API service: sessions, tokens, and the rate limiter in front of
both.
EOF

cat > "$ROOT/src/main.rs" <<'EOF'
mod auth;
mod db;
mod ratelimit;
mod routes;
mod session;

use std::net::SocketAddr;

use anyhow::Context;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let url = std::env::var("DATABASE_URL").context("DATABASE_URL is not set")?;
    let pool = db::pool::connect(&url).await?;
    let limiter = ratelimit::RateLimiter::per_minute(60);

    let app = routes::router(pool, limiter);
    let addr: SocketAddr = "0.0.0.0:8080".parse()?;
    let listener = TcpListener::bind(addr).await?;

    tracing::info!(%addr, "listening");
    axum::serve(listener, app).await?;
    Ok(())
}
EOF

cat > "$ROOT/src/auth.rs" <<'EOF'
use std::time::{SystemTime, UNIX_EPOCH};

use crate::session::Session;

/// The claims we mint, and the ones we are willing to read back.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Claims {
    pub sub: Option<u64>,
    pub exp: u64,
    pub scopes: Vec<String>,
}

impl Claims {
    pub fn expired(&self) -> bool {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        self.exp <= now
    }
}

/// Exchange a refresh token for a session.
///
/// The signature is checked first: a token we cannot verify is not a token,
/// and nothing below this line should ever see one.
pub fn refresh_claims(token: &str, keys: &KeySet) -> Result<Session, AuthError> {
    let header = decode_header(token)?;
    let key = keys.by_kid(header.kid).ok_or(AuthError::UnknownKey)?;
    let claims = verify_signature(token, key)?;

    if claims.sub.is_some() {
        return Ok(Session::from_claims(claims));
    }
    Err(AuthError::NoSubject)
}

pub fn validate_token(token: &str, keys: &KeySet) -> Result<Claims, AuthError> {
    let header = decode_header(token)?;
    let key = keys.by_kid(header.kid).ok_or(AuthError::UnknownKey)?;
    let claims = verify_signature(token, key)?;
    if claims.expired() {
        return Err(AuthError::Expired);
    }
    Ok(claims)
}
EOF

cat > "$ROOT/src/session.rs" <<'EOF'
use crate::auth::Claims;

#[derive(Debug, Clone)]
pub struct Session {
    pub user_id: u64,
    pub scopes: Vec<String>,
    pub issued_at: u64,
}

impl Session {
    pub fn from_claims(claims: Claims) -> Self {
        Self {
            user_id: claims.sub.unwrap_or_default(),
            scopes: claims.scopes,
            issued_at: now_secs(),
        }
    }

    pub fn allows(&self, scope: &str) -> bool {
        self.scopes.iter().any(|s| s == scope)
    }
}

fn now_secs() -> u64 {
    let now = std::time::SystemTime::now();
    now.duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}
EOF

cat > "$ROOT/src/db/pool.rs" <<'EOF'
use std::time::Duration;

use sqlx::postgres::{PgPool, PgPoolOptions};

pub const DEADLINE: Duration = Duration::from_secs(5);

/// One pool for the process. The defaults here are what the p99 complaint is
/// about: every request waits on the same twelve connections.
pub async fn connect(url: &str) -> anyhow::Result<PgPool> {
    let pool = PgPoolOptions::new()
        .max_connections(12)
        .acquire_timeout(DEADLINE)
        .connect(url)
        .await?;
    Ok(pool)
}

pub async fn healthy(pool: &PgPool) -> bool {
    let conn = pool.acquire().await;
    conn.is_ok()
}
EOF

cat > "$ROOT/src/db/mod.rs" <<'EOF'
pub mod pool;
EOF

cat > "$ROOT/src/ratelimit.rs" <<'EOF'
use std::collections::HashMap;
use std::time::Instant;

/// A token bucket per API key, refilled on read.
pub struct RateLimiter {
    per_minute: u32,
    buckets: HashMap<String, Bucket>,
}

struct Bucket {
    hits: u32,
    window_started: Instant,
}

impl RateLimiter {
    pub fn per_minute(per_minute: u32) -> Self {
        Self { per_minute, buckets: HashMap::new() }
    }

    pub fn check(&mut self, key: &str) -> Verdict {
        let bucket = self.buckets.entry(key.to_string()).or_insert(Bucket {
            hits: 0,
            window_started: Instant::now(),
        });
        if bucket.window_started.elapsed().as_secs() >= 60 {
            bucket.hits = 0;
            bucket.window_started = Instant::now();
        }
        bucket.hits += 1;
        if bucket.hits > self.per_minute {
            Verdict::TooMany { retry_after: 60 }
        } else {
            Verdict::Allowed
        }
    }
}

pub enum Verdict {
    Allowed,
    TooMany { retry_after: u64 },
}
EOF

cat > "$ROOT/src/routes.rs" <<'EOF'
use axum::routing::{get, post};
use axum::Router;
use sqlx::PgPool;

use crate::ratelimit::RateLimiter;

pub fn router(pool: PgPool, limiter: RateLimiter) -> Router {
    Router::new()
        .route("/v1/sessions", post(create_session))
        .route("/v1/sessions/:id", get(read_session))
        .route("/v1/tokens/refresh", post(refresh))
        .route("/healthz", get(health))
        .with_state(AppState { pool, limiter })
}

#[derive(Clone)]
pub struct AppState {
    pub pool: PgPool,
    pub limiter: std::sync::Arc<tokio::sync::Mutex<RateLimiter>>,
}
EOF

cat > "$ROOT/tests/api.rs" <<'EOF'
//! End-to-end tests against a running service.

#[tokio::test]
async fn concurrent_refresh_mints_one_session() {
    let app = harness::spawn().await;
    let token = harness::refresh_token_for(1);

    let (a, b) = tokio::join!(app.refresh(&token), app.refresh(&token));

    assert_eq!(a.status(), 200);
    assert_eq!(b.status(), 409, "the second exchange must lose the race");
}

#[tokio::test]
async fn expired_refresh_token_is_rejected() {
    let app = harness::spawn().await;
    let token = harness::expired_token_for(1);

    assert_eq!(app.refresh(&token).await.status(), 401);
}
EOF

git -C "$ROOT" init -q
git -C "$ROOT" config user.email "clip@example.invalid"
git -C "$ROOT" config user.name "Clip"
git -C "$ROOT" add -A
git -C "$ROOT" commit -qm "api: sessions, tokens and the limiter in front of them"

echo "$ROOT"
