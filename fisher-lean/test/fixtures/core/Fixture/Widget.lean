import Lean

open Lean Widget

@[widget_module]
def helloWidget : Widget.Module where
  javascript := "export default function (props) { return 'hello' }"

#widget helloWidget

theorem with_widget : True := by
  trivial
