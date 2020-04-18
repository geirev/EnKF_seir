# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 09:29:20 2020

@author: rafaeljmoraes
"""

from tecplot_loader import reader as tecplot_reader
from tecplot_loader import available_observations, available_variables, variable2output, variable2observation

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
import argparse

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

        
def load_ensemble_from_tecplot_file(tecplot_file_name, variable):
    df = tecplot_reader(tecplot_file_name, variable)
    time = df.iloc[:, 0]
    mean = df.iloc[:, 1]
    std_dev = df.iloc[:, 2]
    ensemble = df.iloc[:, 3:df.columns.size-1]
    
    return time, mean, std_dev, ensemble

def load_observed_data_from_tecplot_file(tecplot_file_name, obs_data_name):
    df = tecplot_reader(tecplot_file_name, obs_data_name)
    time = df.iloc[:, 1]
    obs_data = df.iloc[:, 2]
    std_dev = df.iloc[:, 3]
    
    return time, obs_data, std_dev
    
def plot_ensemble(time, mean, std_dev, ensemble, color, fading):
    ensemble_color = lighter(color, fading)
    plt.plot(time, ensemble, color=ensemble_color)
    plt.plot(time, mean, color=color)
    plt.plot(time, mean+std_dev, color=color, linestyle='dashed')
    plt.plot(time, mean-std_dev, color=color, linestyle='dashed')
        
def plot_observed_data(time, observed_data, std_dev, color):
    plt.errorbar(
            time, 
            observed_data, 
            yerr=std_dev, 
            marker='o', 
            markerfacecolor=color, 
            color=color)
    
def plot_variable(data_source, variable, obs_file_name, obs_name):
    post_file_name  = data_source + variable + '_1.dat'
    prior_file_name = data_source + variable + '_0.dat'
    
    color = variable_colors[variable]
    
    # plot prior data first because, since it is more spread than the posterior
    # it won't be covered by it
    output_variable = variable2output[variable] + '_0'
    time, mean, std_dev, ensemble = load_ensemble_from_tecplot_file(prior_file_name, output_variable)
    prior_fading = 0.9
    print("Ploting prior for: ", variable)
    plot_ensemble(time, mean, std_dev, ensemble, color, prior_fading)
    
    output_variable = variable2output[variable] + '_1'
    time, mean, std_dev, ensemble = load_ensemble_from_tecplot_file(post_file_name, output_variable)
    prior_fading = 0.6
    print("Ploting posterior for: ", variable)
    plot_ensemble(time, mean, std_dev, ensemble, color, prior_fading)
    
    # note: not all variables have observed data
    if (obs_name is not None):
        print("Ploting observation for: ", variable)
        time, observed_data, std_dev = load_observed_data_from_tecplot_file(obs_file_name, obs_name)
        plot_observed_data(time, observed_data, std_dev, color)
    
    patch = mpatches.Patch(color=color, label=variable)
    plt.legend(handles=[patch])

def parse_arguments():
    parser = argparse.ArgumentParser(description='EnKF_seir input arguments')
    
    parser.add_argument(
            "--data_dir", 
            default=r'.\\', 
            type=str, 
            help="The directory where the EnKF tecplot files can be found",
            required=False)
    
    parser.add_argument(
            "--figs_out_dir", 
            default=r'.\\', 
            type=str, 
            help="The directory where the figures will be output",
            required=False)
    
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
            default=300, 
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
   obs_file_name = data_dir + '/obs.dat'
   figures = plot_variables(data_dir, requested_vars, obs_file_name)
   save_figures(figs_out_dir, figures, format, dpi, show)
   
def plot_variables(data_source, variables, obs_file_name):
    figures = {}
    for variable in variables:
        fig = plt.figure()
        plt.xlabel('Time (days)')
        plt.ylabel('Number of people (-)')
        obs_data_name = variable2observation[variable]
        print("Observed data: ", obs_data_name)
        plot_variable(data_source, variable, obs_file_name, obs_data_name)
        figures[variable] = fig
        
    return figures
        
if __name__ == '__main__':
    main()
