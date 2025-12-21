import sqlite3
from pathlib import Path
from typing import List

import pandas as pd
import streamlit as st

ANALYTES: List[str] = [
    "sodium",
    "chloride",
    "potassium_plas",
    "co2_totl",
    "bun",
    "creatinine",
    "calcium",
    "glucose",
]

ANALYTE_DISPLAY = {
    "sodium": "Na",
    "chloride": "Cl",
    "potassium_plas": "K",
    "co2_totl": "CO2",
    "bun": "BUN",
    "creatinine": "Cr",
    "calcium": "Ca",
    "glucose": "Glu",
}

DB_PATH = Path(__file__).resolve().parents[1] / "results" / "label_results.db"


def _cell_html(value: float | str | int | None,
               prior_cmp: int | None,
               post_cmp: int | None) -> str:
    """Return HTML for a table cell with arrows based on comparisons.
    Arrows are positioned tightly in the corners when values increase or decrease."""
    val_str = "" if pd.isna(value) else str(value)
    html = f"<div style='position:relative;'>\n{val_str}"
    if prior_cmp:
        arrow = "&#9650;" if prior_cmp > 0 else "&#9660;"
        color = "cyan" if prior_cmp > 0 else "magenta"
        html += (
            f"<span style='position:absolute;top:-16px;right:-18px;color:{color};"
            "font-size:1.2em'>" f"{arrow}</span>"
        )
    if post_cmp:
        arrow = "&#9650;" if post_cmp > 0 else "&#9660;"
        color = "cyan" if post_cmp > 0 else "magenta"
        html += (
            f"<span style='position:absolute;bottom:-16px;right:-18px;color:{color};"
            "font-size:1.2em'>" f"{arrow}</span>"
        )
    html += "</div>"
    return html


def save_label(row: pd.Series, label: str, labeler: str,
               db_path: Path = DB_PATH) -> None:
    """Save selected ``row`` values and metadata to ``db_path``.

    Parameters
    ----------
    row:
        Row of BMP values to save.
    label:
        Label assigned by the user.
    labeler:
        Name of the person assigning the label.
    db_path:
        Path to the SQLite database file.
    """

    # Gather key columns for saving
    columns: dict[str, object] = {
        "patient_id": row.get("patient_id"),
        "drawn_dt_tm": row.get("drawn_dt_tm"),
    }

    # Collect prior, current and post values for each analyte if present
    for analyte in ANALYTES:
        columns[analyte] = row.get(analyte)
        columns[f"{analyte}_prior"] = row.get(f"{analyte}_prior")
        columns[f"{analyte}_post"] = row.get(f"{analyte}_post")

    # Add the assigned label, labeler name and timestamp
    columns["label"] = label
    columns["labeler"] = labeler
    columns["timestamp"] = pd.Timestamp.utcnow().isoformat()

    df = pd.DataFrame([columns])
    with sqlite3.connect(db_path) as conn:
        df.to_sql("labels", conn, if_exists="append", index=False)


def display_entry(row: pd.Series, labeler: str) -> None:
    """Display a single BMP entry in a 3x8 table with action buttons.

    Parameters
    ----------
    row:
        Row of BMP data to display.
    labeler:
        Name of the person assigning the label.
    """
    st.markdown(
        """
        <style>
        table.label-table {border-collapse:collapse;margin:auto;}
        table.label-table th, table.label-table td {
            border:1px solid #ccc;padding:12px 24px;text-align:center;
        }
        .button-row {display:flex;justify-content:center;gap:4px;margin-top:40px;}
        .flash-message {text-align:center;font-weight:bold;color:#228B22;}
        </style>
        """,
        unsafe_allow_html=True,
    )

    html = ["<table class='label-table'>"]
    header = "<tr><th></th>" + "".join(
        f"<th>{ANALYTE_DISPLAY.get(a, a)}</th>" for a in ANALYTES
    ) + "</tr>"
    html.append(header)
    for label in ["prior", "current", "post"]:
        row_cells = []
        for analyte in ANALYTES:
            if label == "current":
                cur = row.get(analyte, None)
                prior = row.get(f"{analyte}_prior", None)
                post = row.get(f"{analyte}_post", None)
                cmp_prior = None
                cmp_post = None
                if cur is not None and prior is not None:
                    cmp_prior = int(cur > prior) - int(cur < prior)
                if cur is not None and post is not None:
                    cmp_post = int(post > cur) - int(post < cur)
                cell = _cell_html(cur, cmp_prior, cmp_post)
            else:
                val_key = f"{analyte}_{label}" if label != "current" else analyte
                cell = str(row.get(val_key, ""))
            row_cells.append(f"<td>{cell}</td>")
        html.append(
            f"<tr><td><b>{label}</b></td>{''.join(row_cells)}</tr>"
        )
    html.append("</table>")

    st.markdown("\n".join(html), unsafe_allow_html=True)
    st.markdown("<br>", unsafe_allow_html=True)
    _, col_real, col_contam, _ = st.columns([2, 2, 2, 2], gap="small")
    if col_real.button("Real", key="real", use_container_width=True):
        save_label(row, "real", labeler)
        st.session_state.index += 1
        st.rerun()
    if col_contam.button("Contaminated", key="contam", use_container_width=True):
        save_label(row, "contaminated", labeler)
        st.session_state.index += 1
        st.rerun()


def main() -> None:
    """Streamlit app for labeling BMP entries.

    Users upload a data file and enter their name before labeling entries.
    """
    st.title("BMP Labeling Dashboard")

    uploaded = st.file_uploader("Upload BMP TSV or CSV", type=["tsv", "csv"])
    if uploaded is None:
        st.stop()

    labeler = st.text_input("Your Name", key="labeler_name")
    if not labeler:
        st.stop()

    if "index" not in st.session_state:
        st.session_state.index = 0
    df = pd.read_csv(uploaded, sep="\t" if uploaded.name.endswith(".tsv") else ",")
    for base in ["potassium_plas", "creatinine", "calcium"]:
        for suffix in ["", "_prior", "_post"]:
            col = f"{base}{suffix}"
            if col in df.columns:
                df[col] = df[col].round(1)
    if st.session_state.index >= len(df):
        st.write("All entries labeled.")
        st.stop()
    row = df.iloc[st.session_state.index]
    display_entry(row, labeler)


if __name__ == "__main__":
    main()
