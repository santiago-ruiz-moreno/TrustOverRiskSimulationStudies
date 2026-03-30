import pandas as pd
import numpy as np
from scipy import stats
import subprocess
import sys
import statsmodels.api as sm
from statsmodels.formula.api import ols
from tqdm import tqdm
import os


def run_structural_model(observations):
    """Execute StructuralModel.py with specified observations parameter"""
    try:
        subprocess.run(
            [sys.executable, "StructuralModel.py", str(observations)],
            check=True
        )
    except subprocess.CalledProcessError as e:
        print(
            f"Warning: StructuralModel.py failed with exit status {e.returncode}")
        print("Attempting to continue with existing data file...")


def load_simulation_data():
    """Load the generated CSV file"""
    try:
        return pd.read_csv("lottery_simulation_results.csv")
    except FileNotFoundError:
        raise FileNotFoundError(
            "lottery_simulation_results.csv not found. Ensure StructuralModel.py runs successfully.")




def run_ranksum_test(df):
    """
    Mann-Whitney U test (ranksum) for 'sent' variable by treatment group.
    Keeps principals only, one observation per participant_id.
    Returns test statistic, p-value, and summary.
    """
    # Keep principals only
    df_principals = df[df["role"] == "Principal"].copy()
    
    # Keep one observation per participant_id
    df_principals = df_principals.groupby("participant_id").first().reset_index()
    
    # Split 'sent' by treatment groups
    treatments = df_principals["treatment"].unique()
    groups = [df_principals[df_principals["treatment"] == t]["sent"].values for t in treatments]
    
    # Perform Mann-Whitney U test
    statistic, p_value = stats.mannwhitneyu(groups[0], groups[1], alternative="two-sided")
    
    return {
        "statistic": statistic,
        "p_value": p_value,
        #"treatments": list(treatments),
        #"n_group1": len(groups[0]),
        #"n_group2": len(groups[1]),
        #"mean_sent_group1": groups[0].mean(),
        #"mean_sent_group2": groups[1].mean()
    }


def run_agent_consistency_test(df):
    """
    Test agent consistency: check if agents who played lottery B switch from B to A.
    Returns chi-square test p-value for treatment effect on B->A switching.
    """
    # Filter for agents only
    agent_df = df[df["role"] == "Agent"].copy()
    
    # Get most frequently played lottery per participant
    agent_df["lottery_played"] = agent_df.groupby("participant_id")["lottery"].transform(
        lambda x: x.mode()[0] if len(x.mode()) > 0 else x.iloc[0]
    )
    
    # Get choice for the most played lottery
    agent_df["choice_lp"] = agent_df.loc[agent_df["lottery"] == agent_df["lottery_played"], "choice"]
    agent_df["choice_played"] = agent_df.groupby("participant_id")["choice_lp"].transform(
        lambda x: x.mode()[0] if len(x.mode()) > 0 else x.iloc[0]
    )
    
    # Keep one row per participant
    agent_df = agent_df.groupby("participant_id").first().reset_index()
    
    # Keep only those who chose B
    agent_df = agent_df[agent_df["choice_played"] == "B"].copy()
    
    # Define B->A switch
    agent_df["B_to_A"] = (agent_df["toInvest"] == "A").astype(int)
    
    # Chi-square test for treatment effect on B->A switching
    contingency_table = pd.crosstab(agent_df["treatment"], agent_df["B_to_A"])
    chi2, p_value, dof, expected = stats.chi2_contingency(contingency_table)
    
    return {"chi2_statistic": chi2, "p_value": p_value}

def run_regression(df):
    """
    Run regression: rho ~ TotalPayoff * treatment
    Returns: betas for TotalPayoff and interaction terms, p-values
    """
    df = df[df["role"] == "Principal"].copy()
    df = df[df["payoff_lottery"] >= 0].copy()
    df = df[df["lottery"] == 10].copy()
    df = df[df["rho"] > 0].copy()
    df["TotalPayoff"] = df["amount"] - df["sent"] + df["payoff_lottery"]
    df["treatment_num"] = pd.factorize(df["treatment"])[0] + 1
    df["is_llm_advice"] = (df["treatment"] == "LLM-advice").astype(int)

    X = df[["TotalPayoff", "is_llm_advice"]]
    X["interaction"] = df["TotalPayoff"] * df["is_llm_advice"]
    X = sm.add_constant(X)

    y = df["rho"]
    model = sm.OLS(y, X).fit(cov_type="HC1")

    return model.params, model.pvalues, model


def bootstrap_analysis(sample_size, n_simulations=1000):
    
    
    """
    Run bootstrap analysis: sample data, run regression, count significant betas
    
    """
    all_results = []

    for i in range(n_simulations):
        if os.path.exists("lottery_simulation_results.csv"):
            os.remove("lottery_simulation_results.csv")
        run_structural_model(sample_size)
        data = load_simulation_data()
        try:
            params, pvalues, model = run_regression(data)
            chi2_result = run_agent_consistency_test(data)
            ranksum_result = run_ranksum_test(data)

            all_results.append({
                "sample_size": sample_size,
                "simulation": i,
                "is_llm_advice_beta": params.get("is_llm_advice", np.nan),
                "is_llm_advice_pvalue": pvalues.get("is_llm_advice", np.nan),
                "interaction_beta": params.get("interaction", np.nan),
                "interaction_pvalue": pvalues.get("interaction", np.nan),
                "chi2_statistic": chi2_result["chi2_statistic"],
                "chi2_pvalue": chi2_result["p_value"],
                "ranksum_statistic": ranksum_result["statistic"],
                "ranksum_pvalue": ranksum_result["p_value"]
            })

        except Exception as e:
            print(f"Simulation {i} failed: {e}")
            continue

    return all_results

def main():
    sample_sizes = [30, 50, 60, 70, 80, 100, 150, 200, 300]
    n_simulations = 1000
    all_results_compiled = []
    
    # Load existing results if available
    results_file = "bootstrap_results_all.csv"
    if os.path.exists(results_file):
        all_results_compiled = pd.read_csv(results_file).to_dict('records')
        print(f"Loaded {len(all_results_compiled)} existing results")
    
    for sample_size in tqdm(sample_sizes, desc="Sample sizes"):
        # Check how many simulations are already done for this sample size
        existing_count = sum(1 for r in all_results_compiled if r["sample_size"] == sample_size)
        if existing_count >= n_simulations:
            print(f"Sample size {sample_size}: already complete ({existing_count}/{n_simulations})")
            continue
        
        print(f"Running bootstrap analysis with sample size {sample_size} ({existing_count}/{n_simulations} done)...")
        results = bootstrap_analysis(sample_size, n_simulations - existing_count)
        all_results_compiled.extend(results)
        
        # Save after each sample size
        results_df = pd.DataFrame(all_results_compiled)
        results_df.to_csv(results_file, index=False)
    
    # Create summary dataframe with effect sizes
    results_df = pd.DataFrame(all_results_compiled)
    summary_results = []
    for sample_size in sample_sizes:
        df_subset = results_df[results_df["sample_size"] == sample_size]
        sig_is_llm_advice = (df_subset["is_llm_advice_pvalue"] < 0.05).sum()
        sig_interaction = (df_subset["interaction_pvalue"] < 0.05).sum()
        sig_chi2 = (df_subset["chi2_pvalue"] < 0.05).sum()
        sig_ranksum = (df_subset["ranksum_pvalue"] < 0.05).sum()
        
        # Calculate effect sizes
        mean_is_llm_advice_beta = df_subset["is_llm_advice_beta"].mean()
        mean_interaction_beta = df_subset["interaction_beta"].mean()
        
        # Determine effect size labels
        is_llm_effect = "Small" if abs(mean_is_llm_advice_beta) < 0.1 else ("Medium" if abs(mean_is_llm_advice_beta) < 0.3 else "Large")
        interaction_effect = "Small" if abs(mean_interaction_beta) < 0.1 else ("Medium" if abs(mean_interaction_beta) < 0.3 else "Large")
        
        summary_results.append({
            "sample_size": sample_size,
            "is_llm_advice_sig": f"{sig_is_llm_advice}/{n_simulations}",
            "is_llm_advice_pct": f"{100*sig_is_llm_advice/n_simulations:.1f}%",
            "is_llm_advice_effect_size": is_llm_effect,
            "interaction_sig": f"{sig_interaction}/{n_simulations}",
            "interaction_pct": f"{100*sig_interaction/n_simulations:.1f}%",
            "interaction_effect_size": interaction_effect,
            "chi2_sig": f"{sig_chi2}/{n_simulations}",
            "chi2_pct": f"{100*sig_chi2/n_simulations:.1f}%",
            "ranksum_sig": f"{sig_ranksum}/{n_simulations}",
            "ranksum_pct": f"{100*sig_ranksum/n_simulations:.1f}%"
        })
    
    summary_df = pd.DataFrame(summary_results)
    summary_df.to_csv("bootstrap_summary.csv", index=False)
    print(f"\n=== Bootstrap Results Summary ===")
    print(summary_df.to_string(index=False))
    
if __name__ == "__main__":
    main()
