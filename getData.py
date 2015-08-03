from MesoPy import Meso, MesoPyError
import json
import sys

#The script creats one file for each station (stationName.json)
stations = ['SCBH1','KKRH1','KTAH1','PLHH1','C0875','KFWH1','MKRH1','SCEH1','SCSH1','WNVH1']

#Mesowest token
x = Meso('ecbfdf783690489eb29d50b18b700827')

for station in range(len(stations)):
    orig_stdout = sys.stdout
    fileName =  stations[station] + ".json" 

    f = file(fileName, 'w')
    sys.stdout = f
    
    returned = x.timeseries_obs(stid=stations[station], start='201401010000', end='201501010500', obtimezone='local', vars='solar_radiation')

    #returned = x.latest_obs(stid='KKRH1, SCBH1, KTAH1, PLHH1', obtimezone='local', vars='solar_radiation', within='1440' )


    #write the json file
    print (json.dumps(returned, indent = 4))

    sys.stdout = orig_stdout
    f.close()

#Delete the first line of each file, it is necessary because the first line does not make part of the json file
for station in range (len(stations)):
    fileName =  stations[station] + ".json" 
    lines = open(fileName, 'r').readlines()
    lines[0] = "\n"
    file = open(fileName, 'w')
    for line in lines:
        file.write(line)
    file.close()
    
