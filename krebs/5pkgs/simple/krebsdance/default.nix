{ writers }:
writers.writePython3Bin "krebsdance" {} ''
  import argparse
  import random
  import itertools

  claws = [
      dict(
          up="(\\/)",
          down="(/\\)",
          left="(\\\\)",
          right="(//)",
      ),
      dict(
          up="(V)",
          down="(A)",
          left=">)=",
          right="=(<",
      ),
      dict(
          up="(U)",
          down="(n)",
          left=")==",
          right="==(",
      ),
  ]

  eyes = [
      "°",
      "*",
      "^",
      "ö",
      "o",
      "O",
      "X",
      "x",
      "U",
      "u",
  ]

  bodies = [
      dict(
          left="(",
          right=")",
      ),
      dict(
          left="{",
          right="}",
      ),
      dict(
          left="[",
          right="]",
      ),
      dict(
          left="<",
          right=">",
      ),
      dict(
          left="|",
          right="|",
      ),
  ]

  mouths = [
      ",,,,",
      ",mm,",
      "_mm_",
      "-mm-",
      ";;;;",
      ";mm;",
      ":mm:",
      "::::",
      ":ww:",
      ":<>:",
  ]


  def all_krebses():
      for mouth, body, eye, claw in itertools.product(mouths, bodies, eyes, claws):
          yield f'{claw["up"]} {body["left"]}{eye}{mouth}{eye}{body["right"]} {claw["up"]}'


  def krebs_graph() -> str:
      return "\n".join(
          ["digraph {"]
          + [f'"{krebs}"->"{generate(seed=krebs)}"' for krebs in all_krebses()]
          + ["}"]
      )


  def generate(*, seed: str, dancing: bool = False) -> str:
      if seed:
          random.seed(seed)
      clawstyle = random.choice(claws)
      body = random.choice(bodies)
      eye = random.choice(eyes)
      mouth = random.choice(mouths)
      if dancing:
          return "\n".join(
              [
                  f'{clawstyle["down"]} {body["left"]}{eye}{mouth}{eye}{body["right"]}{clawstyle["up"]}',
                  f'{clawstyle["left"]}{body["left"]}{eye}{mouth}{eye}{body["right"]} {clawstyle["right"]}',
                  f'{clawstyle["right"]} {body["left"]}{eye}{mouth}{eye}{body["right"]} {clawstyle["left"]}',
                  f'{clawstyle["down"]}{body["left"]}{eye}{mouth}{eye}{body["right"]}{clawstyle["down"]}',
              ]
          )
      else:
          return f'{clawstyle["up"]} {body["left"]}{eye}{mouth}{eye}{body["right"]} {clawstyle["up"]}'


  def fixpoints():
      for krebs in all_krebses():
          if generate(seed=krebs) == krebs:
              yield krebs


  def main():
      parser = argparse.ArgumentParser()

      parser.add_argument(
          "seed",
          nargs="?",
          help="random seed to use for generating the krebs variant",
      )

      parser.add_argument(
          "--dance",
          "-d",
          dest="dance",
          help="if the krebs should dance",
          default=False,
          action="store_true",
      )

      parser.add_argument(
          "--mode",
          "-m",
          dest="mode",
          choices=["graphviz", "plain"],
          default="plain",
      )

      args = parser.parse_args()

      if args.mode == "plain":
          print(generate(seed=args.seed, dancing=args.dance))
      elif args.mode == "graphviz":
          print(krebs_graph())


  if __name__ == "__main__":
      main()
''
