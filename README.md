# Drumming Styles: A Distributional Feature Analysis

An analysis of drumming style using the Groove MIDI Dataset (Gillick et al., 
2019), applying Principal Component Analysis (PCA) and Kernel Density 
Estimation (KDE) to 446,312 individual drum hits from 10 professional 
drummers to identify latent dimensions of playing style and characterize 
distributional "signatures" in velocity dynamics and timing. Extends to 
Hidden Markov Models (HMM) fitted to representative recordings to explore 
temporal structure within performances. Originally completed as a final 
project for a Fall 2025 UVA STAT 5330 course.

**Key findings**: the first three principal components capture 63.8% of 
variance in drummer features, with PC2 strongly correlating with mean 
velocity (r=0.89). KDE identifies three primary velocity distribution 
typologies among drummers, suggesting discrete dynamic levels rather than 
continuous variation.

## Presentation

[View the presentation slides](https://htmlpreview.github.io/?https://github.com/maxvanzandt/Drumming-Styles-A-Distributional-Feature-Analysis/blob/main/presentation.html)

## Data Source

**Groove MIDI Dataset**, from Gillick, J., Roberts, A., Engel, J., Eck, D., 
& Bamman, D. (2019). *Learning to Groove with Inverse Sequence 
Transformations*. International Conference on Machine Learning.

Available at [magenta.tensorflow.org/datasets/groove](https://magenta.tensorflow.org/datasets/groove).

## Reproducing the Analysis

1. Clone this repository.
2. Download the Groove MIDI Dataset from the link above and unzip it.
3. Place the resulting `groove/` folder (containing `info.csv` and the 
   per-drummer subfolders) directly in the repo root, alongside 
   `Data Load.py`.
4. Install dependencies: `pip install -r requirements.txt`
5. Run `Data Load.py` to parse the raw MIDI files and produce `all_hits.csv` 
   (446,312 rows, one per drum hit). This is the slow step - it processes 
   all 1,150 recordings - and only needs to be run once.
6. Run `analysis.R` to reproduce the PCA, KDE, and HMM analysis.

## Files

- `Data Load.py` - parses the raw Groove MIDI files into a single tidy 
  `all_hits.csv`
- `analysis.R` - PCA, KDE, and HMM analysis (extracted from the report's 
  code appendix)
- `requirements.txt` - Python dependencies for `Data Load.py`
- `report.pdf` - final written report
- `presentation.html` - presentation slides (see link above for a rendered 
  view)

## License

See `LICENSE` (BSD 3-Clause).
