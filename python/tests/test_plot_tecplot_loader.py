import pytest
from .context import enkf_seir
from enkf_seir import plot
from enkf_seir.plot import tecplot_loader
from enkf_seir.plot import plot

import os
import sys
from pathlib import Path

def test_enkf_seir_plot_plot(tmp_path):
    obs = '''TITLE = "Observations"
 VARIABLES = "i" "time" "ave" "std" 
 ZONE T="Observed deaths"  F=POINT, I=   4, J=1, K=1
    1   12      1.0000      2.0000
    2   13      1.0000      2.0000
    3   14      3.0000      2.0000
    4   15      3.0000      2.0000
'''
    file = tmp_path / "obs.dat"
    file.write_text(obs)

    variable = r'Hospitalized'
    with pytest.raises(Exception) as excinfo:
        tecplot_loader.reader(file, variable)
        
    expected_msg = 'Could not load data for variable ' + variable + ' from file ' + str(file) + '.'
    assert expected_msg in str(excinfo.value)