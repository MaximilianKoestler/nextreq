import { createRouter, createWebHashHistory, RouteRecordRaw } from "vue-router";
import Calculator from "../views/Calculator.vue";

const routes: Array<RouteRecordRaw> = [
  {
    path: "/",
    name: "Calculator",
    component: Calculator,
  },
];

const router = createRouter({
  history: createWebHashHistory(),
  routes,
});

export default router;
