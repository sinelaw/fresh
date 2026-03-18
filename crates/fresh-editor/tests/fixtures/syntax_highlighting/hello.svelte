<script lang="ts">
  let name: string = 'World';
  let count: number = 0;

  function greet() {
    count += 1;
    console.log(`Hello, ${name}!`);
  }

  $: message = `Hello, ${name}!`;
</script>

<main>
  <h1>{message}</h1>
  <button on:click={greet}>
    Clicked {count} times
  </button>
</main>

<style>
  h1 {
    color: purple;
    font-size: 2em;
  }
</style>
