{ writers }:
writers.writePython3Bin "krebsdance" {} ''
  import argparse
  import random

  claws = [
      dict(
          up='(\\/)',
          down='(/\\)',
          left='(\\\\)',
          right='(//)',
      ),
      dict(
          up='(V)',
          down='(A)',
          left='>)',
          right='(<',
      ),
      dict(
          up='(U)',
          down='(n)',
          left=')=',
          right='=(',
      ),
  ]

  eyes = [
      '°',
      '*',
      '^',
      'ö',
      '.',
      'o',
      'O',
      'X',
      'x',
      'U',
      'u',
  ]

  bodies = [
      dict(
          left='(',
          right=')',
      ),
      dict(
          left='{',
          right='}',
      ),
      dict(
          left='[',
          right=']',
      ),
      dict(
          left='<',
          right='>',
      ),
      dict(
          left='|',
          right='|',
      ),
  ]

  mouths = [
      ',,,,',
      ',mm,',
      '_mm_',
      '-mm-',
      ';;;;',
      ';mm;',
      ':mm:',
      '::::',
      ':ww:',
      ':<>:',
  ]


  def main():
      parser = argparse.ArgumentParser()

      parser.add_argument(
          'seed',
          nargs='?',
          help='random seed to use for generating the krebs variant',
      )

      parser.add_argument(
          '--dance', '-d',
          dest='dance',
          help='if the krebs should dance',
          default=False,
          action='store_true',
      )

      args = parser.parse_args()

      if args.seed:
          random.seed(args.seed)

      clawstyle = random.choice(claws)
      body = random.choice(bodies)
      eye = random.choice(eyes)
      mouth = random.choice(mouths)
      if args.dance:
          print(f'{clawstyle["down"]} {body["left"]}{eye}{mouth}{eye}{body["right"]}{clawstyle["up"]}')  # noqa
          print(f'{clawstyle["left"]}{body["left"]}{eye}{mouth}{eye}{body["right"]} {clawstyle["right"]}')  # noqa
          print(f'{clawstyle["right"]} {body["left"]}{eye}{mouth}{eye}{body["right"]} {clawstyle["left"]}')  # noqa
          print(f'{clawstyle["down"]}{body["left"]}{eye}{mouth}{eye}{body["right"]}{clawstyle["down"]}')  # noqa
      else:
          print(f'{clawstyle["up"]} {body["left"]}{eye}{mouth}{eye}{body["right"]} {clawstyle["up"]}')  # noqa


  if __name__ == '__main__':
      main()
''
