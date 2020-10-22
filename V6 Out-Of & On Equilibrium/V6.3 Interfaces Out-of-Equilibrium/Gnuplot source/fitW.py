import numpy as np
import scipy
import sys

class File(file):
    """ An helper class for file reading  """

    def __init__(self, *args, **kwargs):
        super(File, self).__init__(*args, **kwargs)
        self.BLOCKSIZE = 4096

    def head(self, lines_2find=1):
        self.seek(0)                            #Rewind file
        return [super(File, self).next() for x in xrange(lines_2find)]

    def tail(self, lines_2find=1):  
        self.seek(0, 2)                         #Go to end of file
        bytes_in_file = self.tell()
        lines_found, total_bytes_scanned = 0, 0
        while (lines_2find + 1 > lines_found and
               bytes_in_file > total_bytes_scanned): 
            byte_block = min(
                self.BLOCKSIZE,
                bytes_in_file - total_bytes_scanned)
            self.seek( -(byte_block + total_bytes_scanned), 2)
            total_bytes_scanned += byte_block
            lines_found += self.read(self.BLOCKSIZE).count('\n')
        self.seek(-total_bytes_scanned, 2)
        line_list = list(self.readlines())
        return line_list[-lines_2find:]

    def backward(self):
        self.seek(0, 2)                         #Go to end of file
        blocksize = self.BLOCKSIZE
        last_row = ''
        while self.tell() != 0:
            try:
                self.seek(-blocksize, 1)
            except IOError:
                blocksize = self.tell()
                self.seek(-blocksize, 1)
            block = self.read(blocksize)
            self.seek(-blocksize, 1)
            rows = block.split('\n')
            rows[-1] = rows[-1] + last_row
            while rows:
                last_row = rows.pop(-1)
                if rows and last_row:
                    yield last_row
        yield last_row

def var( passval ):
   "This prints a passed string into this function"
   return int( (maxtime-1)/float(N-1) *passval + ( (N - maxtime )/float(N-1) ) )



def errorFunction(z,A0,z0,W):
   return A0*scipy.special.erf(math.sqrt(math.pi)*(z-z0)/(2*W))


import csv
import math
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit



folder = "Graphs/"
file = "Inter20L.dat"
output = "FitResults.log"

of = open(output, 'w')
of.write("Time,Roughness\n")

inter = []
#maxtime = []
lista = []
N = 10

with File(file) as f:
	#header = next(f)
	#f.head(1)
	#print f.tail(1)
	lista = f.tail(1)
	for element in lista:
		parts = element.split(',')
		inter.append(parts)
	maxtime = int(inter[0][1])
	ncols = int(inter[0][2])
	print 'maxtime: ' + repr(maxtime)
	print 'ncols: ' + repr(ncols)

if N>maxtime:
	sys.exit("Error! Can't have N>maxtime")	

with open(file) as csvfile:
	readCSV = csv.reader(csvfile, delimiter=',')
	header = next(readCSV)
	#nextLine = next(readCSV)
	t = 1
	variable = var(t)
	#print variable
	ncolIndex = []
	f1up = []
	for i, line in enumerate(readCSV):
		#print variable , int(line[1])
		if (int(line[1]) == variable ):
			ncolIndex.append(int(line[2]))
			f1up.append(float(line[3]))
			if (int(line[2]) == ncols):
				#Fit
				#plt.figure(t)
				#plt.plot(ncolIndex,f1up)
				#plt.show()
				A0 = -1
				z0 = 0
				W = 0.1
				dispXdata = [i - ncols/2 for i in ncolIndex]
				dispYdata = [2*j-1 for j in f1up]
				guess = np.array([A0, z0 , W])
				popt, pcov = curve_fit(errorFunction,dispXdata, dispYdata,p0=guess)
				#print repr(float(line[0]))+","+repr(float(popt[2]))+"\n"
				of.write(repr(int(line[0]))+","+repr(float(popt[2]))+"\n")
				#print popt
				#print pcov
				#plot
				print 'Plot for t = ' + repr(t) + ' time = ' + repr(variable)
				plt.plot(dispXdata,dispYdata,'ro')
				yfit = errorFunction(dispXdata, *popt)
				plt.plot(dispXdata,yfit)
				plt.xlabel('ncols index')
				plt.ylabel('Modif Segregation')
				plt.title('Segregation time t=' + repr(t))
				plt.savefig(folder + 'fig'+repr(t)+'.png', bbox_inches='tight')
				plt.close()
				#clean them up
				ncolIndex = []
				f1up = []
				#update 
				t += 1
				variable = var(t)

of.close()

time = []
Roughness = []
with open(output) as of:
	writeCSV = csv.reader(of, delimiter=',')
	header = next(writeCSV)
	for i, line in enumerate(writeCSV):
		time.append(int(line[0]))
		Roughness.append(float(line[1]))

logtime = [math.log(i) for i in time]
logroughness = [math.log(j) for j in Roughness]
plt.figure(1)
plt.plot(logtime,logroughness,'ro')
plt.xlabel("log time")
plt.ylabel("log W(t)")
plt.title("log Roughness vs Time")
plt.savefig(folder + 'roughness.png', bbox_inches='tight')