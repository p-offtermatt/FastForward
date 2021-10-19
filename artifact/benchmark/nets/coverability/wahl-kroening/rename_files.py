import os


for (dirpath, dirnames, filenames) in os.walk("."):
	for filename in (x for x in filenames if x.endswith(".spec")):
		oldpath = dirpath + "/" + filename
		newpath = dirpath + "/" + dirpath.split("/")[1] + ".spec"
		os.rename(oldpath, newpath)
	

