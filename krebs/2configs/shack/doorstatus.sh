#!/bin/sh
# needs in path:
#   curl jq
# creates and manages $PWD/state
set -euf

send_reaktor(){
  # usage: send_reaktor "text"
  echo "send_reaktor: $1"
  curl -fsS http://localhost:7777 \
      -H content-type:application/json \
      -d "$(jq -n \
        --arg text "$1" '{
          command:"PRIVMSG",
          params:["#shackspace",$text]
        }'
      )"
}

open=$(shuf -n1 <<EOF
happy hacking, shack ist offen
Heureka, der shack ist offen
Die Türe ist offen, der shack will bespielt werden
Frohlocket, der shack ist offen
shack is love, shack is life, shack is offen
Bin da, wer noch? shack hat geöffnet!
shack hat geöffnet: Arbeiten Sie sicher, arbeiten Sie klug!
Bin ich schon drin? Ich bin schon drin.. das war ja einfach. Also im shack.
Uuuuund es setzt sich in Bewegung, wir öffnen den shack, los, los! Ja da guckt ihr, jetzt gehts looos!
EOF
)

close=$(shuf -n1 <<EOF
Hacking vorbei, shack ist zu!
Tja, shack ist zu
Shackie-closie
Der Sandmann kommt, alle shackies sind zu haus und die Tür ist zu
shack hat Stromsparmodus aktiviert
Tür ist zu, shackspace ist jetzt koronakonform
Oh nein, eine Tür, sie ist verschlossen! Also, die vom shack
Ihr kennt das ja: Abschalten. Der shack ist zu.
EOF
)
error=$(shuf -n1 <<EOF
Hase, api ist kaputt! Bitte reparieren
API liefert kein sinnvolles Ergebnis, keine Ahnung ob shack offen oder zu ist
shack api defekt :(
Hubel Hubel, jemand könnte mal die shack api reparieren
API sagt derp
Siehste das? API? Da soll ich jetzt nen Request drauf machen? Jetzt werd ich aber langsam n bisschen wild hier langsam!
Der API ist ein bisschen ein Otto geworden, ischwör der will mich flaxen
ich möchte den geschäftsführer sprechen, das API geht nicht mehr!
Herr makefu an Kasse 3 bitte, Kasse 3 bitte Herr makefu. Der API Computer ist mal wieder ausgefallen
EOF
)

state=$(curl -fSsk https://api.shackspace.de/v1/space | jq  .doorState.open)
prevstate=$(cat state ||:)

if test "$state" == "$(cat state)";then
  #echo "current and last state is the same ($state), doing nothing"
  :
else
  echo "API state and last state differ ( '$state' != '$prevstate')"
  if test "$state" == "true";then
    send_reaktor "$open"
  elif test "$state" == "false";then
    send_reaktor "$close"
  else
    send_reaktor "$error"
  fi
  echo "updating state"
  printf "%s" "$state" > state
fi
