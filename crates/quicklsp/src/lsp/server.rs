//! QuickLSP server implementation.
//!
//! All LSP operations go through a single `Workspace` engine.

use std::path::PathBuf;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::parsing::symbols::SymbolKind as QuickSymbolKind;
use crate::workspace::{SymbolLocation, Workspace};

pub struct QuickLspServer {
    client: Client,
    workspace: Arc<Workspace>,
    workspace_root: Arc<RwLock<Option<PathBuf>>>,
}

impl QuickLspServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            workspace: Arc::new(Workspace::new()),
            workspace_root: Arc::new(RwLock::new(None)),
        }
    }

    fn word_at_position(content: &str, position: Position) -> Option<String> {
        let line = content.lines().nth(position.line as usize)?;
        let col = position.character as usize;
        let chars: Vec<char> = line.chars().collect();
        if col > chars.len() {
            return None;
        }
        let mut start = col;
        let mut end = col;
        while start > 0 && is_ident_char(chars[start - 1]) {
            start -= 1;
        }
        while end < chars.len() && is_ident_char(chars[end]) {
            end += 1;
        }
        if start == end {
            return None;
        }
        Some(chars[start..end].iter().collect())
    }
}

fn is_ident_char(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric()
}

fn aero_kind_to_lsp(kind: QuickSymbolKind) -> SymbolKind {
    match kind {
        QuickSymbolKind::Function => SymbolKind::FUNCTION,
        QuickSymbolKind::Method => SymbolKind::METHOD,
        QuickSymbolKind::Class => SymbolKind::CLASS,
        QuickSymbolKind::Struct => SymbolKind::STRUCT,
        QuickSymbolKind::Enum => SymbolKind::ENUM,
        QuickSymbolKind::Interface => SymbolKind::INTERFACE,
        QuickSymbolKind::Constant => SymbolKind::CONSTANT,
        QuickSymbolKind::Variable => SymbolKind::VARIABLE,
        QuickSymbolKind::Module => SymbolKind::MODULE,
        QuickSymbolKind::TypeAlias => SymbolKind::TYPE_PARAMETER,
        QuickSymbolKind::Trait => SymbolKind::INTERFACE,
        QuickSymbolKind::Unknown => SymbolKind::NULL,
    }
}

fn loc_to_lsp(loc: &SymbolLocation) -> Option<Location> {
    let uri = Url::from_file_path(&loc.file).ok()?;
    Some(Location {
        uri,
        range: Range {
            start: Position {
                line: loc.symbol.line as u32,
                character: loc.symbol.col as u32,
            },
            end: Position {
                line: loc.symbol.line as u32,
                character: (loc.symbol.col + loc.symbol.name.len()) as u32,
            },
        },
    })
}

#[tower_lsp::async_trait]
impl LanguageServer for QuickLspServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(root_uri) = params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                *self.workspace_root.write().await = Some(path);
            }
        }
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "QuickLSP".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "QuickLSP initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if let Ok(path) = params.text_document.uri.to_file_path() {
            self.workspace.index_file(path, params.text_document.text);
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.last() {
            if let Ok(path) = params.text_document.uri.to_file_path() {
                self.workspace.update_file(path, change.text.clone());
            }
        }
    }

    async fn did_close(&self, _params: DidCloseTextDocumentParams) {}

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let source = match self.workspace.file_source_from_uri(uri) {
            Some(s) => s,
            None => return Ok(None),
        };
        let symbol = match Self::word_at_position(&source, pos) {
            Some(s) => s,
            None => return Ok(None),
        };
        let defs = self.workspace.find_definitions(&symbol);
        if let Some(loc) = defs.first().and_then(loc_to_lsp) {
            return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let source = match self.workspace.file_source_from_uri(uri) {
            Some(s) => s,
            None => return Ok(None),
        };
        let symbol = match Self::word_at_position(&source, pos) {
            Some(s) => s,
            None => return Ok(None),
        };
        let refs = self.workspace.find_references(&symbol);
        if refs.is_empty() {
            return Ok(None);
        }
        let locs: Vec<Location> = refs
            .iter()
            .filter_map(|r| {
                let u = Url::from_file_path(&r.file).ok()?;
                Some(Location {
                    uri: u,
                    range: Range {
                        start: Position {
                            line: r.line as u32,
                            character: r.col as u32,
                        },
                        end: Position {
                            line: r.line as u32,
                            character: (r.col + r.len) as u32,
                        },
                    },
                })
            })
            .collect();
        if locs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locs))
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let path = match uri.to_file_path() {
            Ok(p) => p,
            Err(_) => return Ok(None),
        };
        let syms = self.workspace.file_symbols(&path);
        if syms.is_empty() {
            return Ok(None);
        }
        let lsp_syms: Vec<SymbolInformation> = syms
            .iter()
            .map(|s| {
                #[allow(deprecated)]
                SymbolInformation {
                    name: s.name.clone(),
                    kind: aero_kind_to_lsp(s.kind),
                    tags: None,
                    deprecated: None,
                    location: Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position {
                                line: s.line as u32,
                                character: s.col as u32,
                            },
                            end: Position {
                                line: s.line as u32,
                                character: (s.col + s.name.len()) as u32,
                            },
                        },
                    },
                    container_name: None,
                }
            })
            .collect();
        Ok(Some(DocumentSymbolResponse::Flat(lsp_syms)))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        if params.query.is_empty() {
            return Ok(None);
        }
        let results = self.workspace.search_symbols(&params.query);
        if results.is_empty() {
            return Ok(None);
        }
        let syms: Vec<SymbolInformation> = results
            .iter()
            .take(20)
            .filter_map(|loc| {
                let location = loc_to_lsp(loc)?;
                #[allow(deprecated)]
                Some(SymbolInformation {
                    name: loc.symbol.name.clone(),
                    kind: aero_kind_to_lsp(loc.symbol.kind),
                    tags: None,
                    deprecated: None,
                    location,
                    container_name: None,
                })
            })
            .collect();
        if syms.is_empty() {
            Ok(None)
        } else {
            Ok(Some(syms))
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let source = match self.workspace.file_source_from_uri(uri) {
            Some(s) => s,
            None => return Ok(None),
        };
        let partial = match Self::word_at_position(&source, pos) {
            Some(s) if !s.is_empty() => s,
            _ => return Ok(None),
        };
        let results = self.workspace.completions(&partial);
        if results.is_empty() {
            return Ok(None);
        }
        let mut seen = std::collections::HashSet::new();
        let items: Vec<CompletionItem> = results
            .into_iter()
            .filter(|loc| seen.insert(loc.symbol.name.clone()))
            .take(20)
            .map(|loc| CompletionItem {
                label: loc.symbol.name,
                kind: Some(CompletionItemKind::TEXT),
                ..Default::default()
            })
            .collect();
        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }
}
