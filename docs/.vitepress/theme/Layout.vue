<script setup lang="ts">
import { computed } from "vue";
import { useRoute } from "vitepress";
import DefaultTheme from "vitepress/theme";

const { Layout } = DefaultTheme;
const route = useRoute();

// Show the RSS subscribe link on individual blog entries, but not on the
// blog index (which already links to the feed in its own intro). Match on
// the "/blog/" segment so this works regardless of whether route.path
// includes the site base.
const isBlogEntry = computed(() => {
  const p = route.path.replace(/\.html$/, "").replace(/\/$/, "");
  return p.includes("/blog/") && !p.endsWith("/blog");
});
</script>

<template>
  <Layout>
    <template #doc-footer-before>
      <a
        v-if="isBlogEntry"
        class="blog-rss"
        href="/docs/feed.rss"
        target="_blank"
        rel="noopener"
      >
        <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
          <path d="M6.18 17.82a2.18 2.18 0 1 0 0 4.36 2.18 2.18 0 0 0 0-4.36zM4 10.1v3.04a7.86 7.86 0 0 1 7.86 7.86h3.04A10.9 10.9 0 0 0 4 10.1zM4 4v3.04A13.96 13.96 0 0 1 17.96 21H21A16.99 16.99 0 0 0 4 4z" />
        </svg>
        Subscribe via RSS
      </a>
    </template>
  </Layout>
</template>
