import subprocess
import time
import urllib.request
import logging
import argparse
import socket
import struct
import signal
import os

wifiDB = ''
logger = logging.getLogger()
got_signal = False


def signal_handler(signum, frame):
    global got_signal
    got_signal = True


def get_default_gateway() -> str:
    """Read the default gateway directly from /proc."""
    with open("/proc/net/route") as fh:
        for line in fh:
            fields = line.strip().split()
            if fields[1] != '00000000' or not int(fields[3], 16) & 2:
                continue

            return socket.inet_ntoa(struct.pack("<L", int(fields[2], 16)))


def connect(ssid, psk=None):
    subprocess.run(
        ["nmcli", "connection", "delete", "autowifi"],
        stdout=subprocess.PIPE,
    )
    logging.info('connecting to %s', ssid)
    if psk is None:
        subprocess.run(
            [
                "nmcli",
                "device",
                "wifi",
                "connect",
                ssid,
                "name",
                "autowifi",
            ],
            stdout=subprocess.PIPE,
        )
    else:
        subprocess.run(
            [
                "nmcli",
                "device",
                "wifi",
                "connect",
                ssid,
                "name",
                "autowifi",
                "password",
                psk,
            ],
            stdout=subprocess.PIPE,
        )
    time.sleep(5)


def scan():
    logging.debug('scanning wifis')
    wifis_raw = subprocess.check_output([
        "nmcli",
        "-t",
        "device",
        "wifi",
        "list",
        "--rescan",
        "yes",
    ])
    wifis_list = wifis_raw.split(b'\n')
    logging.debug('scanning wifis finished')
    wifis = []
    for line in wifis_list:
        logging.debug(line)
        ls = line.split(b':')
        if len(ls) == 8:
            wifis.append({
                "ssid": ls[1],
                "signal": int(ls[5]),
                "crypto": ls[7]
            })
    return wifis


def get_known_wifis():
    wifis_lines = []
    with open(wifiDB) as f:
        wifis_lines = f.read().splitlines()
    wifis = []
    for line in wifis_lines:
        ls = line.split('/')
        wifis.append({"ssid": ls[0].encode(), "psk": ls[1].encode()})
    return wifis


def check_network():
    logging.debug('checking network')

    global got_signal
    if got_signal:
        logging.info('got disconnect signal')
        got_signal = False
        return False
    else:
        gateway = get_default_gateway()
        if gateway:
            response = subprocess.run(
                [
                    'ping',
                    '-q',
                    '-c',
                    '1',
                    gateway,
                ],
                stdout=subprocess.PIPE,
            )
            if response.returncode == 0:
                logging.debug('host %s is up', gateway)
                return True
            else:
                logging.debug('host %s is down', gateway)
                return False
        else:
            logging.debug('no gateway')
            return False


def check_internet():
    logging.debug('checking internet')

    try:
        with open('./dummy_internet') as f:
            dummy_content = f.read()
            if dummy_content == 'xxx\n':
                return True
        beacon = urllib.request.urlopen('http://krebsco.de/secret')
    except Exception as e:  # noqa
        logging.debug(e)
        logging.info('no internet exc')
        return False
    if beacon.read() == b'1337\n':
        return True
    logging.info('no internet oh')
    return False


def is_wifi_open(wifi):
    if wifi['crypto'] == b'':
        return True
    else:
        return False


def is_wifi_seen(wifi, seen_wifis):
    for seen_wifi in seen_wifis:
        if seen_wifi["ssid"] == wifi["ssid"]:
            return True
    return False


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        '-c', '--config',
        dest='config',
        help='wifi config file to use',
        default='/etc/wifis',
    )

    parser.add_argument(
        '-l', '--loglevel',
        dest='loglevel',
        help='loglevel to use',
        default=logging.INFO,
    )

    parser.add_argument(
        '-p', '--pidfile',
        dest='pidfile',
        help='file to write the pid to',
        default=None,
    )

    args = parser.parse_args()

    global wifiDB
    wifiDB = args.config
    logger.setLevel(args.loglevel)

    signal.signal(signal.SIGUSR1, signal_handler)

    if args.pidfile:
        with open(args.pidfile, 'w+') as f:
            f.write(str(os.getpid()))

    while True:
        if not check_network():
            wifis = scan()
            known_wifis = get_known_wifis()
            known_seen_wifis = [
                wifi for wifi in known_wifis if is_wifi_seen(wifi, wifis)
            ]
            for wifi in known_seen_wifis:
                connect(wifi['ssid'], wifi['psk'])
                if check_network():
                    break
            open_wifis = filter(is_wifi_open, wifis)
            for wifi in open_wifis:
                connect(wifi['ssid'])

                if check_network():
                    break
        time.sleep(10)


if __name__ == '__main__':
    main()
