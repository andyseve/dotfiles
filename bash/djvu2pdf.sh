#!/bin/bash

file = $1
file_name = 
tmp = ($file_name).tiff
djvu = ($file_name).djvu
pdf = ($file_name).pdf
ddjvu -format=tiff ($file).djvu tmp.tiff
tiff2pdf -j -o ($file).pdf tmp.tiff
rm tmp.tiff
