import path from "node:path";
import { existsSync, statSync, writeFileSync } from "node:fs";
import { Feed } from "feed";
import { createContentLoader, type ContentData, type SiteConfig } from "vitepress";

// Public origin the docs site is served from (base path is added per-item).
const hostname = "https://getfresh.dev";

// Map a page url back to its markdown source file so we can fall back to the
// file's modified time when a post has no explicit `date` in frontmatter.
function resolveSourceFile(srcDir: string, url: string): string | undefined {
  const clean = url.replace(/(^\/|\/$)/g, "").replace(/\.html$/, "");
  return [
    path.join(srcDir, clean, "index.md"),
    path.join(srcDir, `${clean}.md`),
  ].find((candidate) => existsSync(candidate));
}

// Publish date for a post: explicit frontmatter `date`, otherwise the source
// file's last-modified time. This guarantees every entry appears in the feed.
function postDate(srcDir: string, { url, frontmatter }: ContentData): Date {
  if (frontmatter.date) return new Date(frontmatter.date as string);
  const file = resolveSourceFile(srcDir, url);
  return file ? statSync(file).mtime : new Date();
}

// Rewrite relative href/src attributes to absolute URLs so images and links
// in the feed content resolve in standalone feed readers.
function absolutizeLinks(html: string, base: string): string {
  return html.replace(
    /(href|src)="(?!https?:\/\/|\/\/|#|mailto:|data:)([^"]*)"/g,
    (_match, attr: string, value: string) =>
      `${attr}="${new URL(value, base).href}"`,
  );
}

export async function genFeed(config: SiteConfig) {
  const base = config.site.base; // e.g. "/docs/"

  const feed = new Feed({
    title: "Fresh Blog",
    description: "Updates on new features and changes in Fresh.",
    id: `${hostname}${base}blog/`,
    link: `${hostname}${base}blog/`,
    language: "en",
    image: `${hostname}${base}logo.svg`,
    favicon: `${hostname}${base}logo.svg`,
    copyright: "Released under the Apache 2.0 License",
  });

  // Only the top-level blog posts (blog/<slug>/index.md), not the nested
  // per-feature sub-pages (blog/<slug>/<feature>/index.md).
  const posts = await createContentLoader("blog/*/index.md", {
    render: true,
  }).load();

  posts
    .map((post) => ({ post, date: postDate(config.srcDir, post) }))
    .sort((a, b) => +b.date - +a.date)
    .forEach(({ post: { url, html, frontmatter }, date }) => {
      // createContentLoader returns route-relative urls (e.g. "/blog/x/");
      // prepend the site base only if it isn't already present.
      const routed = url.startsWith(base)
        ? url
        : path.posix.join(base, url);
      const link = `${hostname}${routed}`;

      feed.addItem({
        title: frontmatter.title,
        id: link,
        link,
        description: frontmatter.description,
        content: html ? absolutizeLinks(html, link) : undefined,
        date,
      });
    });

  writeFileSync(path.join(config.outDir, "feed.rss"), feed.rss2());
}
