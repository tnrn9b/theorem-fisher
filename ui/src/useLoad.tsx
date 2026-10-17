import { useEffect, useState, type ReactNode } from "react";
import { ApiError } from "./api";

export type Load<T> = { state: "loading" } | { state: "error"; error: ApiError | Error } | { state: "ok"; data: T };

/** Run a fetch when the key changes; the latest call wins. */
export function useLoad<T>(fn: () => Promise<T>, key: string): Load<T> {
  const [s, setS] = useState<Load<T>>({ state: "loading" });
  useEffect(() => {
    let live = true;
    setS({ state: "loading" });
    fn().then(
      (data) => live && setS({ state: "ok", data }),
      (error: Error) => live && setS({ state: "error", error }),
    );
    return () => {
      live = false;
    };
    // the key names what fn reads
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key]);
  return s;
}

/** The loading and error states every view shows the same way. */
export function Loaded<T>({ load, children }: { load: Load<T>; children: (data: T) => ReactNode }) {
  if (load.state === "loading") return <p className="muted">Loading...</p>;
  if (load.state === "error") {
    const e = load.error;
    if (e instanceof ApiError && e.status === 401)
      return <p role="alert">Signed out. Open the URL that fisher ui printed to sign in.</p>;
    if (e instanceof ApiError && e.status === 404) return <p role="alert">Not found: {e.message}.</p>;
    return <p role="alert">Could not load: {e.message}</p>;
  }
  return <>{children(load.data)}</>;
}
