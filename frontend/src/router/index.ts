import { createRouter, createWebHashHistory, RouteRecordRaw } from "vue-router";
import TheCalculator from "@/views/TheCalculator.vue";

const routes: Array<RouteRecordRaw> = [
  {
    path: "/",
    name: "TheCalculator",
    component: TheCalculator,
  },
];

const router = createRouter({
  history: createWebHashHistory(),
  routes,
});

export default router;
