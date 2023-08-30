# YASSO database reader

The purpose of the database reader is to:

* Provide a modern, efficient and extendable logic for reading the YASSO database.
* Provide functionality for shaping the data and splitting it into training and testing data.
* Save the shaped data in a clean CSV-format ready for calibration.

## Installation

1. Clone (contributors and developers) or download (users) this repository.

2. Obtain the YASSO database files from the development team and save them to `data/`.

3. Follow the instructions in the [calibration project](https://github.com/YASSOmodel/YASSO-calibration) documentation.

## Usage

Source the reader codes in `R/` to create calibration-ready data to `results/`. See the [calibration project](https://github.com/YASSOmodel/YASSO-calibration) documentation for details.
