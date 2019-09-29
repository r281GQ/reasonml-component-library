[@react.component]
let make = () =>
  <Box p={`mq([`xxl, `xl, `m, `xxs])}> {ReasonReact.string("hye")} </Box>;