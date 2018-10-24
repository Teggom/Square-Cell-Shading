# Square-Cell-Shading
This was an extension of another project. The idea was to mess with pictures. 

given a picture it will break the picture into the largest grid of specified size (powers of 2)
	given a 1000x100 picture, and a threshold size of 64, we would get a grid of 15x1
	the remainder pixels are cut off, it was the easiest thing to do while retaining the power of 2 size

This will go through each 'box' and check color change across it. 
	It checks maximum color difference and pixel color difference vs average pixel color of the cluster. 
If the square fails to meet the threshold it will break it into 4 smaller squares and try again. 
If the pixel threshold is met it will shade the entire box the average color before returning to the previous level

