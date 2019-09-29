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

type box = [ Theme.spacing | `mq(list(Theme.spacing))];

type direction =
  | Right
  | Left
  | Top
  | Bottom;

let convert_direction_to_rule =
  fun
  | Right => Css.paddingRight
  | Left => Css.paddingLeft
  | Top => Css.paddingTop
  | Bottom => Css.paddingBottom;

let createStyleSheet = (value, direction) => {
  let fn = direction |> convert_direction_to_rule;

  value->Belt.Option.mapWithDefault(
    [],
    x => {
      let result =
        switch (x) {
        | #Theme.spacing as spacing => [spacing |> ToCss.spacing_to_css |> fn]
        | `mq(values) =>
          values->Belt.List.mapWithIndex((index, value) =>
            switch (index) {
            | 0 => value |> ToCss.spacing_to_css |> fn
            | 1 =>
              Css.media(
                "(min-width: 40em)",
                [value |> ToCss.spacing_to_css |> fn],
              )
            | 2 =>
              Css.media(
                "(min-width: 52em)",
                [value |> ToCss.spacing_to_css |> fn],
              )
            | _ =>
              Css.media(
                "(min-width: 64em)",
                [value |> ToCss.spacing_to_css |> fn],
              )
            }
          )
        };
      result;
    },
  );
};

[@react.component]
let make = (~children, ~p=?, ~px=?, ~py=?, ~pl=?, ~pr=?, ~pt=?, ~pb=?) => {
  let paddingTop =
    switch (p, py, pt) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let paddingBottom =
    switch (p, py, pb) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let paddingRight =
    switch (p, px, pr) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let paddingLeft =
    switch (p, px, pl) {
    | (_, _, Some(value)) => Some(value)
    | (_, Some(value), None) => Some(value)
    | (Some(value), None, None) => Some(value)
    | (_, _, _) => None
    };

  let leftResult = createStyleSheet(paddingLeft, Left) |> Css.style;
  let rightResult = createStyleSheet(paddingRight, Right) |> Css.style;
  let topResult = createStyleSheet(paddingTop, Top);
  let bottomResult = createStyleSheet(paddingBottom, Bottom);

  let finalResult = Css.merge([leftResult, rightResult]);
  // Belt.List.concatMany([|
  //   leftResult,
  //   rightResult,
  //   topResult,
  //   bottomResult,
  // |]);

  // paddingLeft->Belt.Option.mapWithDefault(
  //   [],
  //   x => {
  //     let result =
  //       switch (x) {
  //       | #Theme.spacing as spacing => [
  //           spacing |> ToCss.spacing_to_css |> Css.paddingLeft,
  //         ]
  //       | `mq(values) =>
  //         values->Belt.List.mapWithIndex((index, value) =>
  //           switch (index) {
  //           | 0 => value |> ToCss.spacing_to_css |> Css.paddingLeft
  //           | 1 =>
  //             Css.media(
  //               "(min-width: 40em)",
  //               [value |> ToCss.spacing_to_css |> Css.paddingLeft],
  //             )
  //           | 2 =>
  //             Css.media(
  //               "(min-width: 52em)",
  //               [value |> ToCss.spacing_to_css |> Css.paddingLeft],
  //             )
  //           | _ =>
  //             Css.media(
  //               "(min-width: 64em)",
  //               [value |> ToCss.spacing_to_css |> Css.paddingLeft],
  //             )
  //           }
  //         )
  //       };
  //     result;
  //   },
  // );

  // let rightResult =
  //   paddingRight->Belt.Option.mapWithDefault(Css.empty([]), x =>
  //     x |> ToCss.spacing_to_css |> Css.paddingRight
  //   );

  // let topResult =
  //   paddingTop->Belt.Option.mapWithDefault(Css.empty([]), x =>
  //     x |> ToCss.spacing_to_css |> Css.paddingTop
  //   );

  // let bottomResult =
  //   paddingBottom->Belt.Option.mapWithDefault(Css.empty([]), x =>
  //     x |> ToCss.spacing_to_css |> Css.paddingBottom
  //   );

  <div className=finalResult> children </div>;
};