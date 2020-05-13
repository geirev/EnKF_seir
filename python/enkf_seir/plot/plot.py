# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 09:29:20 2020

@author: rafaeljmoraes
"""

from tecplot_loader import reader as tecplot_reader
from tecplot_loader import available_observations, available_variables, variable2output, variable2observation

from datetime import datetime, timedelta

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
import argparse
from pathlib import Path

variable_colors = [
        (0, 0, 1), 
        (0, 1, 0),
        (1, 0, 0),
        (1, 1, 0),
        (0, 0, 0),
        (0, 1, 1),
        (1, 0, 1),
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
    
def plot_ensemble(time, mean, std_dev, ensemble, color, fading, tf, daily):
    ensemble_color = lighter(color, fading)
    plt.plot(time, ensemble, color=ensemble_color)
    plt.plot(time, mean, color=color)
    plt.xlim(None, tf)
    if daily == 'true':
        rate = [mean[i + 1] - mean[i] for i in range(len(mean)-1)] 
        rate.insert(0, 0.0)
        plt.vlines(time, np.ones(len(rate)), rate, color=color)
        plt.yscale('log')
        plt.ylim(1, None)
        plt.grid(axis='y')
    #plt.plot(time, mean+std_dev, color=color, linestyle='dashed')
    #plt.plot(time, mean-std_dev, color=color, linestyle='dashed')
        
def plot_observed_data(time, observed_data, std_dev, color):
    plt.errorbar(
            time, 
            observed_data, 
            yerr=std_dev, 
            marker='o', 
            markerfacecolor=color, 
            color=color)
    
def plot_variable(data_source, variable, obs_file_name, obs_name, prior, t0, tf, daily):
    post_file_name  = data_source / (variable + '_1.dat')
    prior_file_name = data_source / (variable + '_0.dat')
    
    color = variable_colors[variable]
    
    # plot prior data first because, since it is more spread than the posterior
    # it won't be covered by it
    if prior == 'true':
        output_variable = variable2output[variable] + '_0'
        time, mean, std_dev, ensemble = load_ensemble_from_tecplot_file(prior_file_name, output_variable)
        if t0 is not None:
            time = [t0 + timedelta(days=elapsed_days) for elapsed_days in time]
        prior_fading = 0.9
        print("Ploting prior for: ", variable)
        plot_ensemble(time, mean, std_dev, ensemble, color, prior_fading, tf, daily)
    
    output_variable = variable2output[variable] + '_1'
    time, mean, std_dev, ensemble = load_ensemble_from_tecplot_file(post_file_name, output_variable)
    if t0 is not None:
        time = [t0 + timedelta(days=elapsed_days) for elapsed_days in time]
        
    prior_fading = 0.6
    print("Ploting posterior for: ", variable)
    plot_ensemble(time, mean, std_dev, ensemble, color, prior_fading, tf, daily)
    
    # note: not all variables have observed data
    if (obs_name is not None):
        print("Ploting observation for: ", variable)
        time, observed_data, std_dev = load_observed_data_from_tecplot_file(obs_file_name, obs_name)
        if t0 is not None:
            time = [t0 + timedelta(days=elapsed_days) for elapsed_days in time]
        plot_observed_data(time, observed_data, std_dev, color)
    
    patch = mpatches.Patch(color=color, label=variable)
    plt.legend(handles=[patch])

def parse_arguments():
    parser = argparse.ArgumentParser(description='EnKF_seir input arguments')
    
    parser.add_argument(
            "--daily", 
            default='false', 
            type=str, 
            help="The initial date in d/m-Y if axis label should be formatted as dates",
            required=False)

    parser.add_argument(
            "--t0", 
            default=None, 
            type=str, 
            help="The initial date in d/m-Y if axis label should be formatted as dates",
            required=False)

    parser.add_argument(
            "--tf", 
            default=None, 
            type=str, 
            help="The final date in d/m-Y. Only applies if t0 is given.",
            required=False)

    parser.add_argument(
            "--data_dir", 
            default='./', 
            type=str, 
            help="The directory where the EnKF tecplot files can be found",
            required=False)
    
    parser.add_argument(
            "--figs_out_dir", 
            default='./', 
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
            "--prior", 
            default='false', 
            type=str, 
            help="Wheter to include the prior ensemble in the plots",
            choices=['true', 'false'],
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
    
    t0 = None
    if args.t0 is not None:
        t0 = datetime.strptime(args.t0, '%d/%m-%Y')

    tf = None
    if args.tf is not None:
        tf = datetime.strptime(args.tf, '%d/%m-%Y')

    return Path(args.data_dir), args.variables, Path(args.figs_out_dir), args.format, args.dpi, args.prior, args.show, t0, tf, args.daily
    
def save_figures(figs_out_dir, figures, format, dpi, show):
    for var, fig in figures.items():
        file_name = var + '.' + format
        try:
            fig.savefig(figs_out_dir / file_name, format=format, dpi=dpi)
        except:
            print("Could not save figure: ", file_name)
        if show == 'true':
            plt.show()
        plt.close(fig)

def plot_save_variables(data_dir, requested_vars, figs_out_dir, format, dpi, prior, show, t0, tf, daily):
    obs_file_name = data_dir / r'obs.dat'
    figures = plot_variables(data_dir, requested_vars, obs_file_name, prior, t0, tf, daily)
    save_figures(figs_out_dir, figures, format, dpi, show)

def main():
   data_dir, requested_vars, figs_out_dir, format, dpi, prior,  show, t0, tf, daily = parse_arguments()
   plot_save_variables(data_dir, requested_vars, figs_out_dir, format, dpi, prior, show, t0, tf, daily)
   
def plot_variables(data_source, variables, obs_file_name, prior, t0, tf, daily):
    figures = {}
    for variable in variables:
        fig = plt.figure()
        plt.ylabel('Number of people (-)')
        if variable == 'Rens':
            plt.ylabel('Reproduction factor R(t)')
        plt.xlabel('Time (days)')
        obs_data_name = variable2observation[variable]
        print("Observed data: ", obs_data_name)
        plot_variable(data_source, variable, obs_file_name, obs_data_name, prior, t0, tf, daily)
        figures[variable] = fig
        
    return figures
        
if __name__ == '__main__':
    main()
