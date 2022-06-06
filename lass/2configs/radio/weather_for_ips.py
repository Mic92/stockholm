import geoip2.database
import fileinput
import json
import requests
import os


geoip = geoip2.database.Reader(os.environ['MAXMIND_GEOIP_DB'])
seen = {}
output = []
for ip in fileinput.input():
    location = geoip.city(ip.strip())
    if location.city.geoname_id not in seen:
        seen[location.city.geoname_id] = True
        weather_api_key = os.environ['OPENWEATHER_API_KEY']
        url = (
            f'https://api.openweathermap.org/data/2.5/onecall'
            f'?lat={location.location.latitude}'
            f'&lon={location.location.longitude}'
            f'&appid={weather_api_key}'
            f'&units=metric'
        )
        resp = requests.get(url)
        weather = json.loads(resp.text)
        output.append(
            f'Weather report for {location.city.name}, {location.country.name}. '
            f'Currently it is {weather["current"]["weather"][0]["description"]} outside '
            f'with a temperature of {weather["current"]["temp"]} degrees, '
            f'and a wind speed of {weather["current"]["wind_speed"]} meters per second. '
            f'The probability of precipitation is {weather["hourly"][0]["pop"] * 100} percent. '
        )

print('\n'.join(output))
