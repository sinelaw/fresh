<script setup lang="ts">
import { computed } from "vue";
import { useRoute, withBase } from "vitepress";
import DefaultTheme from "vitepress/theme";

const rssIcon = withBase("/rss.svg");

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
        <img class="blog-rss-icon" :src="rssIcon" alt="" width="16" height="16" />
        Subscribe via RSS
      </a>
    </template>
  </Layout>
</template>
