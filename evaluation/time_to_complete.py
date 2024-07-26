import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import numpy as np
import pandas as pd
from plotly import graph_objects as go


def process_times(time_list, adjust=False):
    '''
    Process the times to completion

    Parameters:
    -----------
    time_list : list of lists
        List of 2-item lists where first element is time-to-completion in
        minutes and second element is name of the item
    adjust : string
        Name of item to reset to np.nan (used when want line to extend to when
        stopped reproduction, without it marking it as another item complete).
        Default is False (which means nothing is adjusted).

    Returns:
    --------
    plt_times: dataframe
        Processed dataframe of times, ready for plotting
    '''
    # Create times dataframe...

    # Convert to dataframe
    times = pd.DataFrame(time_list, columns=['time', 'item'])

    # Sort the DataFrame by 'time' to correctly plot the ECDF
    times_clean = times.sort_values(by='time').reset_index(drop=True)

    # Compute the ECDF values
    times_clean['ecdf'] = np.arange(
        1, len(times_clean) + 1) / len(times_clean) * 100

    # Amend item to set as np.nan (so time extends to that point)
    if adjust is not False:
        times_clean.loc[times_clean['item'] == adjust, 'ecdf'] = np.nan

    # Insert the starting point (0, 0) in the ECDF
    times_clean.loc[-1] = [0, 'Start', 0]
    times_clean.index = times_clean.index + 1
    times_clean = times_clean.sort_index()

    # Convert time to hours
    times_clean['hours'] = times_clean['time']/60

    # Add points for the 90 degree turns in the line...

    # Adjust index so it goes 0 2 4 6...
    times_original = times_clean.copy()
    times_original.index = times_clean.index*2

    times_extra = times_clean.copy()

    # Get result from row 1 to final, and add another row with np.nan
    next_result = times_clean['time'][1:].reset_index(drop=True)
    next_result[len(next_result)] = np.nan
    # Replace time with those values (i.e. the value from the row below)
    times_extra['time'] = next_result
    times_extra['item'] = np.nan
    # Then adjust index so it goes 1 3 5 7...
    times_extra.index = (times_extra.index*2)+1

    # Combine (so have alternating rows from each due to odd and even indexes)
    plt_times = pd.concat([times_original, times_extra]).sort_index()

    # Convert time to hours
    plt_times['hours'] = plt_times['time']/60

    return plt_times


def success_interactive(times_df):
    '''
    Create interactive plot of time-to-completion

    Parameters:
    -----------
    times_df : dataframe
        Dataframe of times processed for plotting by process_times()
    '''
    fig = go.Figure()
    # Line
    fig.add_trace(go.Scatter(
        x=times_df['hours'],
        y=times_df['ecdf'],
        mode='lines',
        hoverinfo='skip'))
    # Scatter
    items_only = times_df[times_df['item'].notna()].copy()
    fig.add_trace(go.Scatter(
        x=items_only['hours'],
        y=items_only['ecdf'],
        mode='markers',
        marker=dict(color='blue', size=6),
        hovertext=items_only['item'],
        hoverlabel=dict(namelength=0),
        hovertemplate=(
            '%{hovertext}<br>Time: %{x:.1f} hours<br>Completion: %{y:.1f}%')))

    # Amend appearance
    fig.update_layout(
        xaxis=dict(
            title='Time elapsed (hours)',
            range=[0, 40]
        ),
        yaxis=dict(
            title='Percentage of items reproduced',
            range=[0, 100],
            ticksuffix='%'
        ),
        showlegend=False)

    # Show plot, hiding toolbar and not allowing zoom
    fig.layout.xaxis.fixedrange = True
    fig.layout.yaxis.fixedrange = True
    fig.show(config={'displayModeBar': False})


def success_static(times_df):
    '''
    Create static/non-interactive plot of time-to-completion

    Parameters:
    -----------
    times_df : dataframe
        Dataframe of times processed for plotting by process_times()
    '''
    fig, ax = plt.subplots()
    ax.plot(times_df['hours'], times_df['ecdf'], marker='.', markevery=2)
    plt.xlim(0, 40)
    plt.ylim(0, 100)
    plt.xlabel('Time elapsed (hours)')
    plt.ylabel('Percentage of items reproduced')
    ax.yaxis.set_major_formatter(mtick.PercentFormatter())
    plt.show()
