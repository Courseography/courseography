def parse_time_slots(s):
    """ Parse multiple time slots """
    rest = s.strip()
    # Check for 'TBA':
    if rest.upper() == 'TBA':
        return ([], 'TBA')
    else:
        allSlots = []
        while rest:
            slots, newRest = parseTimeSlot(rest)
            allSlots.extend(slots)
            if rest == newRest:
                break
            else:
                if newRest and newRest[0] == ",":
                    rest = newRest[1:]
                else:
                    rest = newRest

        return allSlots, rest

def parseTimeSlot(s):
    """ Parse string s of form M?T?W?R?F?#(-#)? """
    days, rest = parseDays(s)
    times, rest = parseTimes(rest)

    slots = [(d, t) for d in days for t in times]
    return slots, rest

def parseDays(s):
    """ Parse string s of form M?T?W?R?F? """
    rest = s.strip()
    days = []
    while rest and rest[0] in 'MTWRF':
        days.append('MTWRF'.find(rest[0]))
        rest = rest[1:].strip()

    if not days:
        print('Error: no days parsed on string ' + s)

    return days, rest

def parseTimes(s):
    """ Parse string of form #(-#)? """
    start, rest = parseStartTime(s)
    if rest and rest[0] == '-':
        end, rest = parseEndTime(rest[1:])
    else:
        end = start + 1

    time = start
    times = []
    while time < end:
        # TODO: this fails for 1.5 hour courses!
        times.append(time)
        time += 1

    return times, rest

def parseStartTime(s):
    """ Parse a single hour in range 9am-8pm. Converts to 24h time """
    if len(s) > 1 and s[0:2].isdigit():
        time = int(s[0:2])
        rest = s[2:]
    elif len(s) > 0 and s[0].isdigit():
        time = int(s[0])
        rest = s[1:]
    else:
        print('Error: could not parse time on string ' + s)
        return -1, s

    if time < 9:
        time += 12

    if rest.startswith(':30'):
        time += 0.5
        rest = rest[3:]

    return time, rest

def parseEndTime(s):
    """ Parse a single hour in range 11am-10pm. Converts to 24h time """
    if len(s) > 1 and s[0:2].isdigit():
        time = int(s[0:2])
        rest = s[2:]
    elif len(s) > 0 and s[0].isdigit():
        time = int(s[0])
        rest = s[1:]
    else:
        print('Error: could not parse time on string ' + s)
        return

    if time < 11:
        time += 12

    if rest.startswith(':30'):
        time += 0.5
        rest = rest[3:]

    return time, rest


def buildGrid(courseTimes):
    """ courseTimes is a list of (code, list of slots) pairs """

    # New grid
    grid = [[[] for n in range(5)] for m in range(12)]

    for (code, slots) in courseTimes:
        for (day, time) in slots:
            grid[time-9][day].append(code)

    return grid

def renderGrid(grid, file):
    """ Write html representation of grid to file """

    # TODO: style in separate file
    html = "<table border cellpadding=\"6\"><tr><th></th><th>Monday</th><th>Tuesday</th>" 
    html += "<th>Wednesday</th><th>Thursday</th>"
    html += "<th>Friday</th></tr>"

    for time in range(12):
        html += "<tr><td>"
        if time < 3:
            html += str(time + 9) + " A.M.</td>"
        elif time == 3:
            html += "12 P.M.</td>"
        else:
            html += str(time - 3) + " P.M.</td>"
        for day in range(5):
            html += "<td>"
            for course in grid[time][day]:
                html += "<p>" + course + "</p>"
            html += "</td>"
        html+= "</tr>\n"
    html += "</table>"

    with open(file, "w+") as f:
        f.write(html)
