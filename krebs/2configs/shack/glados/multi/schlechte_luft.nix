let
  airlevel = name: threshold: color:
    { alias = "${name} Air trigger ${color}";
      trigger = [
      ];
      action =
        [
          # create spark effect with color
        ];
    };
in
{
  # LED
  switch = [
  ];
  automation =
  [
  ];
}
