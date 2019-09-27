module Theme = {
  type spacing = [ | `xxs | `xs | `s | `m | `l | `xl | `xxl];
};

module ToCss = {
  let spacing_to_css =
    fun
    | `xxs => `px(4)
    | `xs => `px(8)
    | `s => `px(16)
    | `m => `px(24)
    | `l => `px(32)
    | `xl => `px(48)
    | `xxl => `px(72);
};

[@react.component]
let make = (~children, ~p=?, ~px=?, ~py=?, ~pl=?, ~pr=?, ~pt=?, ~pb=?) => {
  let paddingTop: option(Theme.spacing) =
    switch (p, py, pt) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let paddingBottom: option(Theme.spacing) =
    switch (p, py, pb) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let paddingRight: option(Theme.spacing) =
    switch (p, px, pr) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let paddingLeft: option(Theme.spacing) =
    switch (p, px, pl) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let leftResult =
    paddingLeft->Belt.Option.mapWithDefault(Css.empty([]), x =>
      x |> ToCss.spacing_to_css |> Css.paddingLeft
    );

  let rightResult =
    paddingRight->Belt.Option.mapWithDefault(Css.empty([]), x =>
      x |> ToCss.spacing_to_css |> Css.paddingRight
    );

  let topResult =
    paddingTop->Belt.Option.mapWithDefault(Css.empty([]), x =>
      x |> ToCss.spacing_to_css |> Css.paddingTop
    );

  let bottomResult =
    paddingBottom->Belt.Option.mapWithDefault(Css.empty([]), x =>
      x |> ToCss.spacing_to_css |> Css.paddingBottom
    );

  <div
    className={Css.style([leftResult, rightResult, topResult, bottomResult])}>
    children
  </div>;
};