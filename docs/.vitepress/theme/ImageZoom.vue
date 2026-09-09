<script setup lang="ts">
import { nextTick, onMounted, onUnmounted, ref } from "vue";
import { onContentUpdated } from "vitepress";

// Lightbox for screenshots and demo GIFs in the docs. Clicking any image in
// the article body opens it full screen; clicking the enlarged image toggles
// between "fit to screen" and 1:1 pixel size, which is what you want for
// dense terminal screenshots.

const mounted = ref(false);
const open = ref(false);
const src = ref("");
const alt = ref("");
const actualSize = ref(false);
const closeButton = ref<HTMLButtonElement | null>(null);
const stage = ref<HTMLElement | null>(null);

let lastFocused: HTMLElement | null = null;
let scrollLock = "";

function zoomableImage(target: EventTarget | null): HTMLImageElement | null {
  if (!(target instanceof HTMLImageElement)) return null;
  // Linked images keep their link, and .no-zoom is the opt-out.
  if (target.closest("a") || target.closest(".no-zoom")) return null;
  if (!target.closest(".vp-doc")) return null;
  return target;
}

function show(image: HTMLImageElement) {
  lastFocused = document.activeElement as HTMLElement | null;
  src.value = image.currentSrc || image.src;
  alt.value = image.alt;
  actualSize.value = false;
  open.value = true;

  scrollLock = document.body.style.overflow;
  document.body.style.overflow = "hidden";
  nextTick(() => closeButton.value?.focus());
}

function toggleActualSize() {
  actualSize.value = !actualSize.value;
  if (!actualSize.value) return;
  // Start the 1:1 view from the middle of the image rather than its corner.
  nextTick(() => {
    const el = stage.value;
    if (!el) return;
    el.scrollLeft = (el.scrollWidth - el.clientWidth) / 2;
    el.scrollTop = (el.scrollHeight - el.clientHeight) / 2;
  });
}

function close() {
  if (!open.value) return;
  open.value = false;
  document.body.style.overflow = scrollLock;
  lastFocused?.focus();
  lastFocused = null;
}

function onClick(event: MouseEvent) {
  // Leave modified clicks alone so "open in new tab" still works on the image.
  if (
    event.defaultPrevented ||
    event.button !== 0 ||
    event.metaKey ||
    event.ctrlKey ||
    event.shiftKey ||
    event.altKey
  ) {
    return;
  }
  const image = zoomableImage(event.target);
  if (!image) return;
  event.preventDefault();
  show(image);
}

function onKeydown(event: KeyboardEvent) {
  if (open.value) {
    if (event.key === "Escape") {
      event.preventDefault();
      close();
    }
    return;
  }
  // Images are given tabindex below, so Enter/Space opens them too.
  if (event.key !== "Enter" && event.key !== " ") return;
  const image = zoomableImage(event.target);
  if (!image) return;
  event.preventDefault();
  show(image);
}

// Make the zoomable images reachable by keyboard after every page render.
onContentUpdated(() => {
  document.querySelectorAll<HTMLImageElement>(".vp-doc img").forEach((image) => {
    if (!zoomableImage(image)) return;
    image.tabIndex = 0;
    image.setAttribute("role", "button");
    image.setAttribute(
      "aria-label",
      image.alt ? `Zoom image: ${image.alt}` : "Zoom image",
    );
  });
});

onMounted(() => {
  mounted.value = true;
  document.addEventListener("click", onClick);
  document.addEventListener("keydown", onKeydown);
});

onUnmounted(() => {
  document.removeEventListener("click", onClick);
  document.removeEventListener("keydown", onKeydown);
  if (open.value) document.body.style.overflow = scrollLock;
});
</script>

<template>
  <Teleport to="body">
    <Transition name="image-zoom-fade">
      <div
        v-if="mounted && open"
        class="image-zoom"
        role="dialog"
        aria-modal="true"
        :aria-label="alt ? `Zoomed image: ${alt}` : 'Zoomed image'"
        @click.self="close"
      >
        <div class="image-zoom-actions">
          <a
            class="image-zoom-button"
            :href="src"
            target="_blank"
            rel="noopener"
            title="Open image in a new tab"
            @click.stop
            >Open in new tab</a
          >
          <button
            ref="closeButton"
            class="image-zoom-button"
            type="button"
            title="Close (Esc)"
            @click="close"
          >
            Close
          </button>
        </div>
        <div
          ref="stage"
          :class="['image-zoom-stage', { 'is-scrollable': actualSize }]"
          @click.self="close"
        >
          <img
            :class="['image-zoom-image', { 'is-actual-size': actualSize }]"
            :src="src"
            :alt="alt"
            @click.stop="toggleActualSize()"
          />
        </div>
      </div>
    </Transition>
  </Teleport>
</template>

<style>
.vp-doc img {
  cursor: zoom-in;
}

.vp-doc a img,
.vp-doc .no-zoom img {
  cursor: inherit;
}

.vp-doc img:focus-visible {
  outline: 2px solid var(--vp-c-brand-1);
  outline-offset: 3px;
}

.image-zoom {
  position: fixed;
  inset: 0;
  z-index: 200;
  display: flex;
  flex-direction: column;
  background: rgba(0, 0, 0, 0.85);
  backdrop-filter: blur(4px);
}

.image-zoom-actions {
  display: flex;
  justify-content: flex-end;
  gap: 8px;
  padding: 12px 16px;
}

.image-zoom-button {
  padding: 6px 14px;
  border: 1px solid #2d2d2d;
  border-radius: 8px;
  background: #111111;
  font-family: var(--vp-font-family-base);
  font-size: 0.85em;
  color: #d4d4d4 !important;
  text-decoration: none !important;
  cursor: pointer;
  transition: all 0.2s ease;
}

.image-zoom-button:hover {
  border-color: #00d9a3;
  color: #00d9a3 !important;
}

.image-zoom-stage {
  flex: 1;
  min-height: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: auto;
  padding: 0 16px 16px;
}

/* At 1:1 the image can outgrow the stage. Flex centering would push its top
   and left edges out of reach of the scrollbars, so fall back to block flow,
   where auto margins collapse to zero once the image is the wider box. */
.image-zoom-stage.is-scrollable {
  display: block;
}

.image-zoom-image {
  max-width: 100%;
  max-height: 100%;
  border-radius: 8px;
  cursor: zoom-in;
}

.image-zoom-image.is-actual-size {
  display: block;
  max-width: none;
  max-height: none;
  margin: 0 auto;
  cursor: zoom-out;
}

.image-zoom-fade-enter-active,
.image-zoom-fade-leave-active {
  transition: opacity 0.2s ease;
}

.image-zoom-fade-enter-from,
.image-zoom-fade-leave-to {
  opacity: 0;
}
</style>
