#!/usr/bin/env python3
import requests
from urllib.parse import urlparse, parse_qs
import sys
from os.path import join as path_join
import re
from bs4 import BeautifulSoup

def is_cloudflare_download_link(tag):
    return tag.name == 'a' and tag.has_attr('href') and tag.get('href').startswith('https://cloudflare-ipfs.com/ipfs/')

def is_libgen_download_link(tag):
    return tag.name == 'a' and tag.has_attr('href') and tag.get('href').startswith('http://libgen.li/ads.php?md5=')

proxy_settings = {
    "http": "socks5h://localhost:9050",
    "https": "socks5h://localhost:9050"
}

def download_raw(download_url):
    r = requests.get(download_url, proxies=proxy_settings)
    r.raise_for_status()
    # return content and file name
    return (r.content, path_join('downloads', urlparse(download_url).path.split('/')[-1]))

download_cloudflare = download_raw

def download_libgen(download_url):
    r = requests.get(download_url, proxies=proxy_settings)
    r.raise_for_status()
    soup = BeautifulSoup(r.text, 'html.parser')
    download_link = soup.find_all('a', href=re.compile('^http://libgen.li/get.php?md5='))
    return download_raw(download_link[0].get('href'))

download_types = {
    "cloudflare": (is_cloudflare_download_link, download_cloudflare),
    "libgen": (is_libgen_download_link, download_libgen)
}

def download_anna(url):
    r = requests.get(url)
    r.raise_for_status()
    soup = BeautifulSoup(r.text, 'html.parser')
    download_link = soup.find_all('a', href=re.compile('^https://cloudflare-ipfs.com/ipfs/'))
    return download_link[0].get('href')

def main(args):
    download_data, path = download_anna(args[0])
    with open(path, 'wb') as f:
        f.write(download_data)
    print(path)

if __name__ == '__main__':
    main(sys.argv[1:])
