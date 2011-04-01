import pty
import os
import subprocess
import sys
import time

from ansi_colors import *


### 
print_prefix = '\{print'
print_suffix = '\}'
timer_flag = 't'
reset_flag = 'r'
open_flag = 'o'


def default_filter(line):
    return


def run(args, filter=default_filter, epilog=''):
    cmd = ['maude'] + args
    #if os.name == 'posix':
    #    cmd = ['python', 'bin/run_pty.py'] + cmd;

    tell_list = [sys.stdout.tell()]
    #print "Loading Maude .......",
    #sys.stdout.flush()
    os.write(sys.stdout.fileno(), "Loading Maude ....... ")
    os.fsync(sys.stdout.fileno())
    tell_list += [sys.stdout.tell()]
    start = time.time()

    if os.name == 'posix':
        """
        (master, slave) = pty.openpty()
        maude = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=slave,
                                 close_fds=True)
        maude_out = os.fdopen(master, 'r')
        """
        maude = subprocess.Popen(['python', 'bin/run_pty.py'] + cmd,
                                 stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        maude_out = maude.stdout
    else:
        maude = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE)
        maude_out = maude.stdout
    maude.stdin.close()

    while True:
        #line = maude.stdout.readline()
        line = maude_out.readline()
        if line.startswith("Bye"):
            end = time.time()
            elapsed = round(end - start, 3)
            time_display = yellow_color + '%.3f' % elapsed + 's' + no_color
            #print epilog + '[' + time_display + ']'
            tell_list += [sys.stdout.tell()]
            os.write(sys.stdout.fileno(), epilog + '[' + time_display + ']\n')
            os.fsync(sys.stdout.fileno())
            tell_list += [sys.stdout.tell()]
            break

        print_suffix_index = line.find(print_suffix)
        if line.startswith(print_prefix) and print_suffix_index != -1:
            content = line[print_suffix_index + len(print_suffix):].rstrip('\n')
            format = line[len(print_prefix):print_suffix_index].strip(' ')

            isTimer = isReset = isOpen = False
            isFormat = True
            for c in format:
                if c == timer_flag:
                    isTimer = True
                elif c == reset_flag:
                    isReset = True
                elif c == open_flag:
                    isOpen = True
                else:
                    isFormat = False
            if not isFormat:
                filter(line)
                continue

            end = time.time()
            elapsed = round(end - start, 3)
            if isReset: start = end

            formated_line = content
            #print content,
            if isTimer:
                time_display = yellow_color + '%.3f' % elapsed + 's' + no_color
                formated_line += ' [' + time_display + ']'
                #print '[' + time_display + ']',
            if not isOpen:
                #print# sys.stdout.tell()
                #print# sys.stdout.tell()
                formated_line += '\n'
            else:
                formated_line += ' '
            while formated_line:
                tell_list += [sys.stdout.tell()]
                n = os.write(sys.stdout.fileno(), formated_line)
                formated_line = formated_line[n:]
                os.fsync(sys.stdout.fileno())
                tell_list += [sys.stdout.tell()]
        else:
            filter(line)
    maude.wait()
    print
    print tell_list

