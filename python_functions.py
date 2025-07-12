import os
import pandas as pd
from avro.datafile import DataFileReader
from avro.io import DatumReader

def empatica_to_csv_memory(avro_dir):
    eda_all = []
    bvp_all = []
    for filename in os.listdir(avro_dir):
        if filename.endswith('.avro'):
            filepath = os.path.join(avro_dir, filename)
            reader = DataFileReader(open(filepath, 'rb'), DatumReader())
            data = next(reader)
            reader.close()
            eda = data['rawData']['eda']
            freq_eda = eda['samplingFrequency']
            if freq_eda > 0 and eda['values']:
                start_eda = eda['timestampStart']
                eda_timestamps = [round(start_eda + i * (1e6 / freq_eda)) for i in range(len(eda['values']))]
                eda_df = pd.DataFrame({'unix_timestamp': eda_timestamps, 'eda': eda['values']})
                eda_all.append(eda_df)
            bvp = data['rawData']['bvp']
            freq_bvp = bvp['samplingFrequency']
            if freq_bvp > 0 and bvp['values']:
                start_bvp = bvp['timestampStart']
                bvp_timestamps = [round(start_bvp + i * (1e6 / freq_bvp)) for i in range(len(bvp['values']))]
                bvp_df = pd.DataFrame({'unix_timestamp': bvp_timestamps, 'bvp': bvp['values']})
                bvp_all.append(bvp_df)
    eda_concat = pd.concat(eda_all).sort_values('unix_timestamp').reset_index(drop=True) if eda_all else pd.DataFrame()
    bvp_concat = pd.concat(bvp_all).sort_values('unix_timestamp').reset_index(drop=True) if bvp_all else pd.DataFrame()
    return {'eda': eda_concat, 'bvp': bvp_concat}
