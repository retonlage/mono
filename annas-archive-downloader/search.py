#!/usr/bin/env python3
import requests
import sys
import re
import json
from bs4 import BeautifulSoup

def search(query):
    url_encoded_query = query.replace(' ', '+')
    url = f'https://annas-archive.org/search?index=&q={url_encoded_query}&sort='
    r = requests.get(url)
    r.raise_for_status()
    soup = BeautifulSoup(r.text, 'html.parser')
    answer_links = soup.find_all('a', href=re.compile('^/md5/'))

    return [{"name": link.find('h3').text.strip().replace("\n", ""), "download_url": f"https://annas-archive.org{link.get('href')}"} for link in answer_links]

def main(args):
    query = ' '.join(args)
    answers = search(query)
    print(json.dumps(answers))

if __name__ == '__main__':
    main(sys.argv[1:])
