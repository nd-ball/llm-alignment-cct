
import argparse
import os
import pandas as pd

# --- Defaults --------------------------------------------------------------

DEFAULT_QUESTIONS_PATH = 'Question_Details.xlsx'
DEFAULT_ROOT = 'Data'

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


# --- Helpers ---------------------------------------------------------------

def build_domain_folder(key: str, country_name: str, root: str) -> str:
    if '_' in key:
        base, suffix = key.split('_', 1)
        return os.path.join(root, base, suffix, country_name)
    return os.path.join(root, key, country_name)


def load_domain_keys(questions_path: str) -> list:
    print(f"Loading domain keys from {questions_path} ...")
    xl = pd.ExcelFile(questions_path)
    print(f"  Found {len(xl.sheet_names)} domains: {xl.sheet_names}")
    return xl.sheet_names


# --- Main ------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument('--questions', default=DEFAULT_QUESTIONS_PATH,
                        help=f'Path to dfquestions Excel (default: {DEFAULT_QUESTIONS_PATH})')
    parser.add_argument('--root', default=DEFAULT_ROOT,
                        help=f'Root data directory (default: {DEFAULT_ROOT})')
    args = parser.parse_args()

    domain_keys = load_domain_keys(args.questions)

    total_written = 0
    skipped = []
    column_mismatches = []

    for key in domain_keys:
        for alpha, country_name in COUNTRIES_SELECTED.items():
            folder = build_domain_folder(key, country_name, args.root)
            human_path = os.path.join(folder, 'human.csv')
            llm_path = os.path.join(folder, '10LLMs.csv')
            out_path = os.path.join(folder, 'human_10LLMs.csv')

            if not os.path.exists(human_path):
                skipped.append((key, country_name, 'missing human.csv'))
                continue
            if not os.path.exists(llm_path):
                skipped.append((key, country_name, 'missing 10LLMs.csv'))
                continue

            human_df = pd.read_csv(human_path)
            llm_df = pd.read_csv(llm_path)

            # Verify column schemas match before concatenating
            if list(human_df.columns) != list(llm_df.columns):
                column_mismatches.append((
                    key, country_name,
                    list(human_df.columns),
                    list(llm_df.columns),
                ))
                # Align LLM columns to human column order; missing cols become NaN
                llm_df = llm_df.reindex(columns=human_df.columns)

            combined = pd.concat([human_df, llm_df], axis=0, ignore_index=True)
            combined.to_csv(out_path, index=False)
            total_written += 1

    print(f"\nDone. Wrote {total_written} combined CSVs.")

    if skipped:
        print(f"\nSkipped {len(skipped)} (key, country) pairs:")
        for s in skipped:
            print(f"  {s}")

    if column_mismatches:
        print(f"\n{len(column_mismatches)} column mismatches (aligned to human schema):")
        for key, country, h_cols, l_cols in column_mismatches:
            h_only = set(h_cols) - set(l_cols)
            l_only = set(l_cols) - set(h_cols)
            print(f"  [{key}/{country}] human-only: {h_only}, llm-only: {l_only}")


if __name__ == '__main__':
    main()