#$python3 -i fitting.py
#>>_

#If module not installed: sudo pip3 install PackageName
import numpy as np
import matplotlib.pyplot as plt


Folder = "../L=256/"
File = "fitmagnet.data"

#Destination = print(Folder + File)
Destination = Folder+File

with open(Destination) as f:
	content = f.readlines()
# you may also want to remove whitespace characters like `\n` at the end of each line
content = [x.strip() for x in content] 
#Size
len(content)
#Element 0 is the title

#{} indicates it is a set. Its elements are not repeated
temps = {float(x.split()[0]) for x in content if x.split()[0]!='Temperatura'}
print('Temp values: ',temps)

FitFile = 'fitted.data'
Subfolder = 'Graphs/'
#Overwrite file
fitf = open(Folder+Subfolder+FitFile,'w')
fitf.write('Temperature Magnetization Magnetization2\n')


#Fit,plot & save the values
for i in range(len(temps)):#

	#list(temps) Transforms a Set to a List because the first does not allow indexing
	#print(x,list(temps)[x])
	#0 0.8
	#1 0.6
	#2 0.3
	#3 0.2
	#4 0.7
	#5 0.5
	#6 0.4	
	currentTemp = list(temps)[i]
	if currentTemp > 1.0:
		continue
	#Create empty lists
	BField = []
	Magnetization = []
	Magnetization2 = []
	print(currentTemp)
	for j in content:
		if j.split()[0] == 'Temperatura':
			continue
		elif float(j.split()[0]) == currentTemp:
			#print(j.split()[0])
			BField.append(float(j.split()[1]))
			Magnetization.append(float(j.split()[2]))
			Magnetization2.append(float(j.split()[3]))
			#print(j.split()[0])
	#The elements for a given temperature were filled
	#-----------------------
	#Fit of Magnetization^2
	# p(x) = p[0] * x**deg + ... + p[deg]
	#Yo quiero p[deg]
	#Transform the x values to array for better slicing
	x = np.array(BField)
	y = np.array(Magnetization)
	y2 = np.array(Magnetization2)
	if currentTemp == 0.3:
		cond = (x<=0.15)
	elif currentTemp > 0.1 and currentTemp < 0.3:
		cond = (x<=0.1)
	elif currentTemp == 0.1:
		cond = (x<=0.05)
	elif currentTemp > 0.3:
		cond = (x>=0.0)
	elif currentTemp < 0.05 and currentTemp>0.01:
		cond = (x<=0.025)
	elif currentTemp == 0.01:
		cond = (x<=0.01)
	else:
		continue

	BArray = x[cond]
	Mag = y[cond]
	Mag2 = y2[cond]

	z = np.polyfit(BArray, Mag2, deg=2)
	f2 = np.poly1d(z)
	#Generate the fitted function
	x_new = np.linspace(BField[0], BField[-1], 50)
	y_new2 = f2(x_new)
	
	#Fit of Magnetization
	w = np.polyfit(BArray, Mag, deg=1)
	f1 = np.poly1d(w)
	#Generate the fitted function
	y_new = f1(x_new)

	#Plot
	f, axarr = plt.subplots(2, sharex=True)
	axarr[0].plot(BField,Magnetization,'bo',label='Magnetización')
	axarr[0].plot(x_new, y_new,'-r',label='Ajuste lineal')
	#plt.set_title('Fitting for T='+str(currentTemp))
	axarr[0].legend(loc='upper left')
	axarr[0].set_title('Magnetizacion')
	axarr[1].plot(BField,Magnetization2,'go',label='Magnetizacion^2')
	axarr[1].plot(x_new, y_new2,'-r',label='Ajuste cuadrático')
	axarr[1].legend(loc='upper left')
	axarr[1].set_title("Magnetizacion^2")
	axarr[1].grid(color='b', alpha=0.5, linestyle='dashed', linewidth=0.5)
	axarr[0].grid(color='b', alpha=0.5, linestyle='dashed', linewidth=0.5)
	plt.xlabel("Campo magnético externo")

	plt.savefig(Folder+Subfolder+'fittingT='+str(currentTemp)+'.pdf', dpi=300)
	#Append the data
	fitf.write(str(currentTemp)+"\t"+str(w[1])+"\t"+str(z[2])+"\n")
fitf.close()	
