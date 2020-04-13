# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 09:29:20 2020

@author: rafaeljmoraes
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
import argparse

available_variables = [
        'dead',
        'hosp',
        'case',
        'susc',
        'recov',
        'infec',
        'expos',
        ]

variable_colors = [
        (0, 0, 1), 
        (0, 1, 0),
        (1, 0, 0),
        (1, 1, 0),
        (0, 0, 0),
        (0, 1, 1),
        (1, 0, 1),
        ]

variable_colors = dict(zip(available_variables, variable_colors))

def lighter(color, percent):
    '''assumes color is rgb between (0, 0, 0) and (1, 1, 1)'''
    color = np.array(color)
    white = np.array([1, 1, 1])
    vector = white-color
    return color + vector * percent
    
def load(file_name, header, skipfooter):
    data = pd.read_csv(
            filepath_or_buffer=file_name, 
            delim_whitespace=True, 
            header=header, 
            skipfooter=skipfooter)
    
    return data
        
def load_ensemble_from_tecplot_file(file_name):
    #TODO: the header footer lines are hardcoded 
    header=53
    skipfooter=33
    df = load(file_name, header, skipfooter)
    time = df.iloc[:, 0]
    mean = df.iloc[:, 1]
    std_dev = df.iloc[:, 2]
    ensemble = df.iloc[:, 3:df.columns.size-1]
    
    return time, mean, std_dev, ensemble

def load_observed_data_from_tecplot_file(file_name):
    #TODO: the header footer lines are hardcoded 
    header=421
    skipfooter=0
    df = load(file_name, header, skipfooter)
    time = df.iloc[:, 0]
    mean = df.iloc[:, 1]
    std_dev = df.iloc[:, 2]
    obs_data = df.iloc[:, 3]
    
    return time, mean, std_dev, obs_data
    
def plot_ensemble(time, mean, std_dev, ensemble, color, fading):
    ensemble_color = lighter(color, fading)
    plt.plot(time, ensemble, color=ensemble_color)
    plt.plot(time, mean, color=color)
    plt.plot(time, mean+std_dev, color=color, linestyle='dashed')
    plt.plot(time, mean-std_dev, color=color, linestyle='dashed')
        
def plot_observed_data(time, mean, std_dev, observed_data, color):
    plt.errorbar(
            time, 
            observed_data, 
            yerr=std_dev, 
            marker='o', 
            markerfacecolor=color, 
            color=color)
    
def plot_variable(data_source, variable):
    post_file_name  = data_source + variable + '_1.dat'
    prior_file_name = data_source + variable + '_0.dat'
    obs_file_name   = data_source + variable + '_0.dat'
    
    color = variable_colors[variable]
    
    # plot prior data first becasue, since it is more spread than the posterior
    # it won't be cover it
    time, mean, std_dev, ensemble = load_ensemble_from_tecplot_file(prior_file_name)
    prior_fading = 0.9
    plot_ensemble(time, mean, std_dev, ensemble, color, prior_fading)
    
    time, mean, std_dev, ensemble = load_ensemble_from_tecplot_file(post_file_name)
    prior_fading = 0.6
    plot_ensemble(time, mean, std_dev, ensemble, color, prior_fading)
    
    # not all variables have observed data
    # TODO: better error handling
    try:
        time, mean, std_dev, observed_data = load_observed_data_from_tecplot_file(obs_file_name)
        plot_observed_data(time, mean, std_dev, observed_data, color)
    except:
        print("Could not parse observed data")
    
    patch = mpatches.Patch(color=color, label=variable)
    plt.legend(handles=[patch])

def parse_arguments():
    parser = argparse.ArgumentParser(description='EnKF_seir input arguments')
    
    parser.add_argument(
            "--data_dir", 
            default=None, 
            type=str, 
            help="The directory where the EnKF tecplot files can be found",
            required=True)
    
    parser.add_argument(
            "--figs_out_dir", 
            default=None, 
            type=str, 
            help="The directory where the figures will be output",
            required=True)
    
    parser.add_argument(
            "--format", 
            default='png', 
            type=str, 
            help="The format the figures will be saved",
            required=False)
    
    parser.add_argument(
            "--show", 
            default='false', 
            type=str, 
            help="Wheter the plots should be shown in interactive sessions",
            choices=['true', 'false'],
            required=False)
    
    parser.add_argument(
            "--dpi", 
            default=1200, 
            type=int, 
            help="The resolution the figures will be saved.",
            required=False)
    
    parser.add_argument(
            "--variables", 
            default=available_variables, 
            nargs='+',
            help="The variables to be plot",
            required=False,
            choices=available_variables)
    
    args = parser.parse_args()
        
    return args.data_dir, args.variables, args.figs_out_dir, args.format, args.dpi, args.show
    
def save_figures(figs_out_dir, figures, format, dpi, show):
    for var, fig in figures.items():
        fig.savefig(figs_out_dir + var + '.' + format, format=format, dpi=dpi)
        if show == 'true':
            plt.show()
        plt.close(fig)
        
def main():
   data_dir, requested_vars, figs_out_dir, format, dpi, show = parse_arguments()
   figures = plot_variables(data_dir, requested_vars)
   save_figures(figs_out_dir, figures, format, dpi, show)
   
def plot_variables(data_source, variables):
    figures = {}
    for variable in variables:
        fig = plt.figure()
        plt.xlabel('Time (days)')
        plt.ylabel('Number of people (-)')
        plot_variable(data_source, variable)
        figures[variable] = fig
        
    return figures
        
if __name__ == '__main__':
    main()
