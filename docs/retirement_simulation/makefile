all : pics/inflation.png pics/inflation_1024.png pics/inflation_512.png pics/stocks_ranges.png pics/stocks_ranges_1024.png pics/stocks_ranges_512.png pics/stocks_paths.png pics/stocks_paths_1024.png pics/stocks_paths_512.png

pics/inflation.png : ../../monte_carlo/inflation.png
	cp ../../monte_carlo/inflation.png pics/inflation.png

pics/inflation_1024.png : pics/inflation.png
	magick pics/inflation.png -resize 1024x pics/inflation_1024.png

pics/inflation_512.png : pics/inflation.png
	magick pics/inflation.png -resize 512x pics/inflation_512.png

pics/stocks_paths.png : ../../monte_carlo/stocks_paths.png
	cp ../../monte_carlo/stocks_paths.png pics/stocks_paths.png

pics/stocks_paths_1024.png : pics/stocks_paths.png
	magick pics/stocks_paths.png -resize 1024x pics/stocks_paths_1024.png

pics/stocks_paths_512.png : pics/stocks_paths.png
	magick pics/stocks_paths.png -resize 512x pics/stocks_paths_512.png


pics/stocks_ranges.png : ../../monte_carlo/stocks_ranges.png
	cp ../../monte_carlo/stocks_ranges.png pics/stocks_ranges.png

pics/stocks_ranges_1024.png : pics/stocks_ranges.png
	magick pics/stocks_ranges.png -resize 1024x pics/stocks_ranges_1024.png

pics/stocks_ranges_512.png : pics/stocks_ranges.png
	magick pics/stocks_ranges.png -resize 512x pics/stocks_ranges_512.png

clean:
	rm -f *~
