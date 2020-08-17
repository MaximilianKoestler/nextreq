<template>
  <div>
    <div class="container">
      <h1>Start typing a calculation</h1>
      <div class="row">
        <div class="col-12"><Calculator /></div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent } from "vue";
import Calculator from "@/components/Calculator.vue";

export default defineComponent({
  name: "TheCalculator",
  components: {
    Calculator,
  },
});
</script>
