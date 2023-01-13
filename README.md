# MR_for_MKs
Probabilistic mass-radius relation for M/K dwarfs (up to 0.7 Solar masses)

Derived in Kipping et al. (2023), "M-Dwarf Exoplanets With Similar Sizes and Instellations to Earth Typically Follow Near-Circular Orbits"

The hyper-parameters governing the mass radius relation are given in columns 1, 2 and 3 of mr_correlated.dat. These are b_perp, theta and V, see the original paper for details. The 4th column is log-likelihood.

The example_polynomial directory is a MultiNest plug-in directory you can use to duplicate the posterior. The model_vs_data_plot.nb plots the results versus Mann's input training data (Mann2015_Tables5_6_7.txt) - see (and give credit to!) https://ui.adsabs.harvard.edu/abs/2015ApJ...804...64M

The most useful file is probably the example_use.nb which reads in the MR posterior + some mass posterior (in the paper we generate these using Mann's code: https://github.com/awmann/M_-M_K-) and calculates the radius and mean density posterior.
