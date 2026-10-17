import { useEffect, useState } from "react";
import { Link, NavLink, Navigate, Route, Routes } from "react-router-dom";
import Context from "./views/Context";
import GoalBoard from "./views/GoalBoard";
import LiveSession from "./views/LiveSession";
import Runs from "./views/Runs";
import Session from "./views/Session";
import Sessions from "./views/Sessions";
import Settings from "./views/Settings";

type Conn = "checking" | "connected" | "signed out" | "offline";

function usePing(): Conn {
  const [state, setState] = useState<Conn>("checking");
  useEffect(() => {
    fetch("/api/ping", { credentials: "same-origin" })
      .then((r) => setState(r.ok ? "connected" : r.status === 401 ? "signed out" : "offline"))
      .catch(() => setState("offline"));
  }, []);
  return state;
}

function Bar() {
  const link = usePing();
  return (
    <header className="bar">
      <Link to="/sessions" className="brand">
        Fisher
      </Link>
      <nav>
        <NavLink to="/sessions">Sessions</NavLink>
        <NavLink to="/search">Search</NavLink>
        <NavLink to="/runs">Runs</NavLink>
        <NavLink to="/settings">Settings</NavLink>
      </nav>
      <span className="conn" data-testid="link" data-state={link}>
        {link === "connected" ? "" : `Server: ${link}`}
      </span>
      {link === "signed out" && <span className="conn">Open the URL that fisher ui printed to sign in.</span>}
    </header>
  );
}

function NotFound() {
  return (
    <main>
      <h1>Not found</h1>
      <p>
        <Link to="/sessions">Back to the sessions</Link>
      </p>
    </main>
  );
}

export default function App() {
  return (
    <>
      <Bar />
      <Routes>
        <Route path="/" element={<Navigate to="/sessions" replace />} />
        <Route path="/sessions" element={<Sessions />} />
        <Route path="/session/:id" element={<Session />} />
        <Route path="/live/:id" element={<LiveSession />} />
        <Route path="/context/:id" element={<Context />} />
        <Route path="/search" element={<GoalBoard />} />
        <Route path="/search/:run" element={<GoalBoard />} />
        <Route path="/runs" element={<Runs />} />
        <Route path="/runs/:bench" element={<Runs />} />
        <Route path="/settings" element={<Settings />} />
        <Route path="*" element={<NotFound />} />
      </Routes>
    </>
  );
}
