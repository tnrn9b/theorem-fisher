import { useState, type FormEvent } from "react";
import { api, type Settings as SettingsData, type SettingsWrite } from "../api";
import { Loaded, useLoad } from "../useLoad";
import css from "./settings.module.css";

const strategyNotes: Record<string, string> = {
  compress: "summarize older turns when the context fills",
  clm: "the context as a file the model edits",
  rlm: "a recursive model over the context",
};

const providers = ["auto", "openrouter", "anthropic", "openai-codex", "custom"];

/**
 * The settings a run started from this page uses: config.yaml's model,
 * provider, autoformalize model and context strategy, and the credentials
 * in .env (shown by their last 4 characters only; a new value replaces
 * the old one and is never shown back).
 */
export default function Settings() {
  const load = useLoad(() => api.settings(), "settings");
  return (
    <main>
      <h1>Settings</h1>
      <p className="muted">
        Read from and written to config.yaml and .env in the Fisher home, as <code>fisher config set</code> does. Runs
        started after a save use the new values.
      </p>
      <Loaded load={load}>{(s) => <Form initial={s} />}</Loaded>
    </main>
  );
}

function Form({ initial }: { initial: SettingsData }) {
  const [base, setBase] = useState(initial);
  const [provider, setProvider] = useState(initial.provider);
  const [model, setModel] = useState(initial.model);
  const [autoModel, setAutoModel] = useState(initial.autoformalizeModel);
  const [strategy, setStrategy] = useState(initial.contextStrategy);
  const [keys, setKeys] = useState<Record<string, string>>({});
  const plainOf = (x: SettingsData) => Object.fromEntries((x.plain ?? []).map((p) => [p.name, p.value]));
  const [plain, setPlain] = useState<Record<string, string>>(plainOf(initial));
  const [busy, setBusy] = useState(false);
  const [status, setStatus] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  const submit = (e: FormEvent) => {
    e.preventDefault();
    const w: SettingsWrite = {};
    if (provider !== base.provider) w.provider = provider;
    if (model !== base.model) w.model = model;
    if (autoModel !== base.autoformalizeModel) w.autoformalizeModel = autoModel;
    if (strategy !== base.contextStrategy) w.contextStrategy = strategy;
    const basePlain = plainOf(base);
    const ks = [
      ...Object.entries(keys).filter(([, v]) => v.trim() !== ""),
      ...Object.entries(plain).filter(([n, v]) => v.trim() !== "" && v.trim() !== (basePlain[n] ?? "")),
    ].map(([name, value]) => ({ name, value }));
    if (ks.length) w.keys = ks;
    setStatus(null);
    setError(null);
    if (!Object.keys(w).length) {
      setStatus("Nothing changed.");
      return;
    }
    setBusy(true);
    api.saveSettings(w).then(
      (s) => {
        setBase(s);
        setProvider(s.provider);
        setModel(s.model);
        setAutoModel(s.autoformalizeModel);
        setStrategy(s.contextStrategy);
        setKeys({});
        setPlain(plainOf(s));
        setBusy(false);
        setStatus("Saved.");
      },
      (err: Error) => {
        setBusy(false);
        setError(`Could not save: ${err.message}`);
      },
    );
  };

  return (
    <form className={css.form} onSubmit={submit} aria-label="Settings">
      {base.warnings.length > 0 && (
        <p className={css.warn}>
          {base.warnings.join(" ")}
        </p>
      )}
      <fieldset className={css.group}>
        <legend>Native agent</legend>
        <label>
          Provider
          <input list="providers" value={provider} onChange={(e) => setProvider(e.target.value)} spellCheck={false} />
        </label>
        <datalist id="providers">
          {providers.map((p) => (
            <option key={p} value={p} />
          ))}
        </datalist>
        <label>
          Model
          <input value={model} onChange={(e) => setModel(e.target.value)} spellCheck={false} />
        </label>
        <label>
          Context strategy
          <select value={strategy} onChange={(e) => setStrategy(e.target.value)}>
            {base.strategies.map((s) => (
              <option key={s} value={s}>
                {s}: {strategyNotes[s] ?? s}
              </option>
            ))}
          </select>
        </label>
      </fieldset>

      <fieldset className={css.group}>
        <legend>Managed workflows</legend>
        <label>
          Autoformalize model
          <input
            value={autoModel}
            onChange={(e) => setAutoModel(e.target.value)}
            placeholder="each backend's own default"
            spellCheck={false}
          />
        </label>
      </fieldset>

      <fieldset className={css.group}>
        <legend>Keys</legend>
        <table className={css.keys}>
          <tbody>
            {base.keys.map((k) => (
              <tr key={k.name} data-testid={`key-${k.name}`}>
                <th scope="row" className={css.mono}>
                  {k.name}
                </th>
                <td className={css.mono}>
                  {!k.set ? "not set" : k.last4 ? `set, ends in ${k.last4}` : "set (too short to show its end)"}
                </td>
                <td>
                  <input
                    type="password"
                    autoComplete="off"
                    aria-label={`New value for ${k.name}`}
                    placeholder="new value"
                    value={keys[k.name] ?? ""}
                    onChange={(e) => setKeys({ ...keys, [k.name]: e.target.value })}
                  />
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </fieldset>

      {(base.plain ?? []).length > 0 && (
        <fieldset className={css.group} data-testid="plain">
          <legend>Other variables</legend>
          <p className="muted">In .env too, but no secrets: shown as they are.</p>
          {(base.plain ?? []).map((p) => (
            <label key={p.name} className={css.mono}>
              {p.name}
              <input
                aria-label={p.name}
                value={plain[p.name] ?? ""}
                placeholder="not set"
                onChange={(e) => setPlain({ ...plain, [p.name]: e.target.value })}
                spellCheck={false}
              />
            </label>
          ))}
        </fieldset>
      )}

      <fieldset className={css.group} data-testid="auto">
        <legend>Search pre-pass</legend>
        <p className="muted">
          Not in config.yaml: these are the defaults of <code>fisher search</code>, changed per run with{" "}
          <code>--auto-tactics</code>, <code>--auto-seconds</code>, <code>--no-repair</code>, <code>--no-disprove</code>{" "}
          and <code>--no-auto</code>.
        </p>
        <dl className={css.auto}>
          <dt>Tactics</dt>
          <dd className={css.mono}>{base.auto.tactics.join(", ")}</dd>
          <dt>Seconds per hole</dt>
          <dd>{base.auto.seconds}</dd>
          <dt>Repair</dt>
          <dd>{base.auto.repair ? "on" : "off"}</dd>
          <dt>Disprove</dt>
          <dd>{base.auto.disprove ? "on" : "off"}</dd>
        </dl>
      </fieldset>

      <div className={css.actions}>
        <button type="submit" disabled={busy}>
          Save
        </button>
        {status && <span role="status">{status}</span>}
        {error && (
          <span role="alert" className={css.error}>
            {error}
          </span>
        )}
      </div>
    </form>
  );
}
