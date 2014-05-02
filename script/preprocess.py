#!/usr/bin/python
import os
fread = open('hourcopy.csv','r')
fwrite = open('hour3.csv','w')
lines = fread.readlines()

weather = {'1':'Clear','2':'Mist','3':'Snow','4':'Rain'}
season = {'1':'Spring','2':'Summer','3':'Autumn','4':'Winter'}

print lines[0]
fwrite.write(','.join(lines[0].split(',')[2:]))
for line in lines[1:]:
	tokens = line.split(',')
	tokens = tokens[2:]
	tokens[0] = season[tokens[0]]
	tokens[1] = 'y' + tokens[1]
	tokens[2] = 'm' + tokens[2]
	tokens[3] = 'h' + tokens[3]
	if(tokens[4] == '0'):
		tokens[4] = 'No'
	else:
		tokens[4] = 'Yes'
	tokens[5] = 'w' + tokens[5]
	if(tokens[6] == '0'):
		tokens[6] = 'No'
	else:
		tokens[6] = 'Yes'
	tokens[7] = weather[tokens[7]]
	print ','.join(tokens)
	fwrite.write(','.join(tokens))
