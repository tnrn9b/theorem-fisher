import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// `npm run build` writes dist/, which fisher-ui embeds at compile time.
// `npm run dev` proxies /api to a running `fisher ui --no-open` on 8732.
export default defineConfig({
  plugins: [react()],
  base: "/",
  build: { outDir: "dist", emptyOutDir: true, sourcemap: false },
  server: { proxy: { "/api": { target: "http://127.0.0.1:8732", changeOrigin: true } } },
});
