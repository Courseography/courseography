import csv
from urllib.request import urlopen
from bs4 import BeautifulSoup
from ttparser import TimetableParser

timetablePath = '../../res/fastimetable2014.csv'

fasTimetableDir = 'http://www.artsandscience.utoronto.ca/ofr/timetable/winter/'
fasTimetableList = fasTimetableDir + 'sponsors.htm'
courses = []

data_map = {
  'code': 0,
  'session': 1,
  'title': 2,
  'section': 3,
  'time': 5,
  'instructor': 7
}

def downloadTimetables():
  print('Downloading timetable list...')
  fas = urlopen(fasTimetableList)
  fasData = fas.read()

  fasSoup = BeautifulSoup(fasData)

  with open(timetablePath, 'w+', encoding='utf8', newline='') as csvfile:
    for li in fasSoup.find_all('li'):
      # Hack for ENG
      if li.a['href'] == 'http://www.english.utoronto.ca/undergrad/2014-15_Timetable.htm':
        html = fasTimetableDir + 'eng.html'
      else:
        html = fasTimetableDir + li.a['href']

      try:
        htmlData = urlopen(html).read()
      except Exception:
        print('Couldn\'t read from ', html)
        continue

      soup = BeautifulSoup(htmlData)

      rows = soup.find_all('tr')

      for row in rows:
        cols = row.find_all('td')
        newcols = []
        for col in cols:
          newcols.append(col.get_text(strip=True))
          x = col.get('colspan')
          if x is not None:
            newcols = newcols + (int(x) - 1) * ['']
        cols = newcols
        if len(cols) == 0 or cols[0] == 'Course' or (len(cols[0]) != 8 and (len(cols) < 6 or not cols[5])):
          continue

        csv.writer(csvfile, dialect='excel').writerow(cols)


def parse_fas_timetable():
  parser = TimetableParser(downloadTimetables, data_map, 'fastimetable2014.csv')
  parser.run()