<template>
  <div>
    <Calculator />
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
