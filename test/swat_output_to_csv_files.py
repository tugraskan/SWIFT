import pandas as pd

output_separator = "\t"

# Default spec for files following the format:
# header, column names, units, then data.
default_spec = {
    "skiprows": [0, 2],   # Skip the general header and the units row.
    "header": 0,          # Use the row after skipping as the header.
    "column_names": None,
    "delim_whitespace": True
}

# Dictionary for new output files using relative paths.
spec_dict = {
    "aqu.out": default_spec,
    "basin_ch.out": default_spec,
    "basin_ex.out": default_spec,
    "cha.out": default_spec,
    "cha_bank.out": default_spec,
    "cha_bed.out": default_spec,
    "cha_fp.out": default_spec,
    "drareas.out": default_spec,
    "hru_ex.out": default_spec,
    "hyd.out": default_spec,
    "lsu_ex.out": default_spec,
    "out.out": default_spec,
    "res.out": default_spec,
    "res_flo.out": default_spec,
    "ru.out": default_spec,
}

def read_output(fname, spec_dict, write_csv=False):
    print(f"Processing file: {fname}")
    if fname in spec_dict:
        spec = spec_dict[fname]
        skiprows = spec["skiprows"]
        header = spec["header"]
        column_names = spec["column_names"]
        delim_whitespace = spec["delim_whitespace"]
        try:
            if header is not None:
                df = pd.read_csv(fname, skiprows=skiprows, delim_whitespace=delim_whitespace, header=header)
            elif header is None and column_names is not None:
                df = pd.read_csv(fname, skiprows=skiprows, delim_whitespace=delim_whitespace, names=column_names)
        except FileNotFoundError:
            print(f"File {fname} not found.")
            exit(1)
        except Exception as e:
            print(f"File {fname} could not be parsed: {e}")
            exit(1)
        if write_csv and df is not None:
            output_file_name = fname + ".csv"
            df.to_csv(output_file_name, sep=output_separator, index=False)
    else:
        print("Filename not found in the specification dictionary.")
        exit(1)
    return df

def run():
    files_to_process = [
        "aqu.out",
        "basin_ch.out",
        "basin_ex.out",
        "cha.out",
        "cha_bank.out",
        "cha_bed.out",
        "cha_fp.out",
        "drareas.out",
        "hru_ex.out",
        "hyd.out",
        "lsu_ex.out",
        "out.out",
        "res.out",
        "res_flo.out",
        "ru.out"
    ]
    
    for fname in files_to_process:
        df = read_output(fname, spec_dict, write_csv=True)
        print(f"Preview of {fname}:")
        print(df.head())
        print("\n" + "-"*40 + "\n")

if __name__ == "__main__":
    run()