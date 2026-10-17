import Lean

open Lean Widget

/-- A widget module other files can show. -/
@[widget_module]
def shared : Widget.Module where
  javascript := "export default function (props) { return 'shared' }"
