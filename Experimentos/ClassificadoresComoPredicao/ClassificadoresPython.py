#!/usr/bin/python
#!python

#import os
#from os import listdir
#from os.path import isfile, join
from sklearn import svm

def testesSVM01():

	X = [[0, 0], [1, 1]]
	y = [0, 1]
	clf = svm.SVC()
	clf.fit(X, y)  

	svm.SVC(C=1.0, cache_size=200, class_weight=None, coef0=0.0, degree=3, gamma=0.0, kernel='rbf', max_iter=-1, 
			probability=False, random_state=None, shrinking=True, tol=0.001, verbose=False)
	
	teste = clf.predict([[2., 2.]])
	print(teste[0])
pass



if __name__ == "__main__":
	
	 testesSVM01()
pass
