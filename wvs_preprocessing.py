import argparse
import os
import pandas as pd
 

DEFAULT_WVS_PATH = 'WVS_Cross-National_Wave_7_csv_v6_0.csv'
DEFAULT_QUESTIONS_PATH = 'Question_Details.xlsx'
DEFAULT_OUTPUT_ROOT = 'Data'
 
COUNTRIES_SELECTED = {
    'ARM': 'Armenia',
    'COL': 'Colombia',
    'DEU': 'Germany',
    'GRC': 'Greece',
    'JPN': 'Japan',
    'MYS': 'Malaysia',
    'MEX': 'Mexico',
    'NLD': 'Netherlands',
    'PER': 'Peru',
    'USA': 'United_States',
}
 
 

def build_domain_path(key: str, country_name: str, root: str) -> str:
    if '_' in key:
        base, suffix = key.split('_', 1)
        return os.path.join(root, base, suffix, country_name, 'human.csv')
    return os.path.join(root, key, country_name, 'human.csv')
 
 
def clean_ev_qids(qids: list) -> list:
    cleaned, seen = [], set()
    for q in qids:
        base = q[:-1] if q.endswith(('A', 'B')) else q
        if base not in seen:
            seen.add(base)
            cleaned.append(base)
    return cleaned
 
 
def load_inputs(wvs_path: str, questions_path: str):
    print(f"Loading WVS from {wvs_path} ...")
    wvs = pd.read_csv(wvs_path, low_memory=False)
    print(f"  WVS shape: {wvs.shape}")
 
    print(f"Loading dfquestions from {questions_path} ...")
    dfquestions = pd.read_excel(questions_path, sheet_name=None)  # dict
    print(f"  dfquestions has {len(dfquestions)} sheets: {list(dfquestions.keys())}")
 
    return wvs, dfquestions
 
 

def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument('--wvs', default=DEFAULT_WVS_PATH,
                        help=f'Path to WVS CSV (default: {DEFAULT_WVS_PATH})')
    parser.add_argument('--questions', default=DEFAULT_QUESTIONS_PATH,
                        help=f'Path to dfquestions Excel (default: {DEFAULT_QUESTIONS_PATH})')
    parser.add_argument('--out', default=DEFAULT_OUTPUT_ROOT,
                        help=f'Output root directory (default: {DEFAULT_OUTPUT_ROOT})')
    args = parser.parse_args()
 
    wvs, dfquestions = load_inputs(args.wvs, args.questions)
 
    total_written = 0
    skipped = []
 
    for key, sub_df in dfquestions.items():
 
        # Skip row 0 (header/label row), keep the rest of the QIDs
        qids = sub_df['QID'].iloc[1:].tolist()
 
        # EV special case: collapse paired IDs
        if key == 'EV':
            qids = clean_ev_qids(qids)
 
        # Sanity check: warn if any QID is missing from wvs
        missing = [q for q in qids if q not in wvs.columns]
        if missing:
            print(f"[{key}] WARNING: {len(missing)} QID(s) not in wvs: {missing}")
            qids = [q for q in qids if q in wvs.columns]
 
        cols_to_keep = ['D_INTERVIEW'] + qids
 
        for alpha, country_name in COUNTRIES_SELECTED.items():
            filtered = wvs.loc[wvs['B_COUNTRY_ALPHA'] == alpha, cols_to_keep]
 
            if filtered.empty:
                skipped.append((key, alpha, country_name))
                print(f"[{key}] No rows for {alpha} ({country_name}) - skipping.")
                continue
 
            out_path = build_domain_path(key, country_name, args.out)
            os.makedirs(os.path.dirname(out_path), exist_ok=True)
            filtered.to_csv(out_path, index=False)
            total_written += 1
 
    print(f"\nDone. Wrote {total_written} CSVs.")
    if skipped:
        print(f"Skipped {len(skipped)} (key, country) pairs due to empty filter:")
        for s in skipped:
            print(f"  {s}")
 
 
if __name__ == '__main__':
    main()