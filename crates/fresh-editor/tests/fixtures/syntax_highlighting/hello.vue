<template>
  <div class="greeting">
    <h1>{{ message }}</h1>
    <button @click="greet">Click me</button>
    <ul>
      <li v-for="item in items" :key="item.id">
        {{ item.name }}
      </li>
    </ul>
  </div>
</template>

<script setup lang="ts">
import { ref, computed } from 'vue'

const name = ref('World')
const items = ref([
  { id: 1, name: 'First' },
  { id: 2, name: 'Second' },
])

const message = computed(() => `Hello, ${name.value}!`)

function greet() {
  console.log(message.value)
}
</script>

<style scoped>
.greeting {
  color: blue;
  font-size: 16px;
}
</style>
