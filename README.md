# haskell

Simple haskell project for processing and prining out nn data from http://yann.lecun.com/exdb/mnist/  
To use run 

    readImageFile >>= (printMatrix 8) 

, where 8 is the number of digit data you want to display and can be in range of 0-9999  
Notice that the picture will be in far right, (I had no reason to translate it, it may be to far to be seen at first, use multiple f's), and you can manipulate it like:  
a - previous digit  
b - next digit  
f - translate picture to the left by 50 pixels  
d - translate picture to the right by 50 pixels  
c - translate picture down by 50 pixels  
x - translate picture up by 50 pixels  
