import numpy as np
import pandas as pd
from dataclasses import dataclass
from typing import Tuple, List
from enum import Enum
import random


# Value for the parameters 

# Using a non-linear utility function for principals to decide how much to send yields more realistic results 
r_paramater: float = 0.88

class Outcome(Enum):
    """Possible outcomes for each option"""
    HIGH = "high"
    LOW = "low"


LOTTERY_CHOICES = {
    1: {
        "A": {"high": 3, "low": -1, "prob_high": 0.95},
        "B": {"low": 2.5, "high": 2.7, "prob_high": 0.05}
    },
    2: {
        "A": {"high": 6, "low": -2, "prob_high": 0.95},
        "B": {"low": 5.0, "high": 5.4, "prob_high": 0.05}
    },
    3: {
        "A": {"high": 9, "low": -3, "prob_high": 0.95},
        "B": {"low": 7.5, "high": 8.1, "prob_high": 0.05}
    },
    4: {
        "A": {"high": 12, "low": -4, "prob_high": 0.95},
        "B": {"low": 10.0, "high": 10.8, "prob_high": 0.05}
    },
    5: {
        "A": {"high": 15, "low": -5, "prob_high": 0.95},
        "B": {"low": 12.5, "high": 13.5, "prob_high": 0.05}
    },
    6: {
        "A": {"high": 18, "low": -6, "prob_high": 0.95},
        "B": {"low": 15.0, "high": 16.2, "prob_high": 0.05}
    },
    7: {
        "A": {"high": 21, "low": -7, "prob_high": 0.95},
        "B": {"low": 17.5, "high": 18.9, "prob_high": 0.05}
    },
    8: {
        "A": {"high": 24, "low": -8, "prob_high": 0.95},
        "B": {"low": 20.0, "high": 21.6, "prob_high": 0.05}
    },
    9: {
        "A": {"high": 27, "low": -9, "prob_high": 0.95},
        "B": {"low": 22.5, "high": 24.3, "prob_high": 0.05}
    },
    10: {
        "A": {"high": 30, "low": -10, "prob_high": 0.95},
        "B": {"low": 25.0, "high": 27.0, "prob_high": 0.05}
    }
}


def evaluate_lottery_choice(lottery_num: int, lambda_: float = 0, weight_loss_: float = 1, f=lambda x: x**0.8, overconfidence: float = None) -> dict:
    """
    Evaluates expected utility for lottery choices A and B using prospect theory weighting.
    
    Args:
        lottery_num: Lottery number (1-10)
        lambda_: Loss aversion parameter.
        weight_loss_: Weight for losses in prospect theory. If None, randomly drawn from [3, 7] when applicable
        f: Utility function (default: identity function)
        overconfidence: Overconfidence parameter (If in treatment the overconfidence will lessen loss aversion effects)
    Returns:
        Dictionary with expected utilities and choice recommendation
    """

    def u(x, lambda_val):
        if x >= 0:
            return f(x)
        else:
            return - lambda_ * f(abs(x))
        
    lottery = LOTTERY_CHOICES[lottery_num]
    p = lottery["A"]["prob_high"]
    
    u_a_high = u(lottery["A"]["high"], lambda_)
    u_a_low = u(lottery["A"]["low"], lambda_)
    #print(u_a_low)

    
    if lottery["A"]["low"] < 0:
        w_plus_a = p
        # In our case only for the low outcome of option A we have an overwieghting of l
        w_minus_a = weight_loss_ * (1 - p)
    else:
        w_plus_a = p
        w_minus_a = 1 - p
    
    
    eu_a = w_plus_a * u_a_high + w_minus_a * u_a_low
    
    u_b_high = u(lottery["B"]["high"], lambda_)
    u_b_low = u(lottery["B"]["low"], lambda_)
    
    w_plus_b = lottery["B"]["prob_high"]
    w_minus_b = 1 - lottery["B"]["prob_high"]
    
    
    eu_b = w_plus_b * u_b_high + w_minus_b * u_b_low
    
    # print("eu_a:", eu_a, "eu_b:", eu_b, "lambda:", lambda_, "weight_loss:", weight_loss_)
    # ADDING NOISE TO THE DECISION
    error = np.random.normal(0, 0.5)
    choice = "A" if eu_a + error >= eu_b  else "B"
    
    return {
        "lottery": lottery_num,
        "lambda": lambda_,
        "eu_a": eu_a,
        "eu_b": eu_b,
        "choice": choice,
        "weight_loss": weight_loss_
    }

def evaluate_lottery_choice_trust_over_risk(lottery_num: int, lambda_: float = None, weight_loss_: float = None, f=lambda x: x**0.8, overconfidence: float = 0) -> dict:
    """
    Evaluates expected utility for lottery choices A and B using prospect theory weighting.
    
    Args:
        lottery_num: Lottery number (1-10)
        lambda_: Loss aversion parameter. If None, randomly drawn from [0, 3]
        weight_loss_: Weight for losses in prospect theory. If None, randomly drawn from [3, 7] when applicable
        f: Utility function (default: identity function)
        overconfidence: Overconfidence parameter (If in treatment the overconfidence will lessen loss aversion effects)
    Returns:
        Dictionary with expected utilities and choice recommendation
    """
    
    
    
    def u(x, lambda_val):
        if x >= 0:
            return f(x)
        else:
            return - lambda_val*overconfidence*f(abs(x))
    
    if lottery_num == 0:
        return {
            "lottery": lottery_num,
            "lambda": lambda_,
            "eu_a": None,
            "eu_b": None,
            "choice": "No choice",
            "weight_loss": weight_loss_
        }
    
    lottery = LOTTERY_CHOICES[lottery_num]
    p = lottery["A"]["prob_high"]
    
    u_a_high = u(lottery["A"]["high"], lambda_)
    u_a_low = u(lottery["A"]["low"], lambda_)
    

    if lottery["A"]["low"] < 0:
        w_plus_a = p
        # In our case only for the low outcome of option A we have an overwieghting of l
        w_minus_a = weight_loss_ * (1 - p)
    else:
        w_plus_a = p
        w_minus_a = 1 - p
    
    eu_a = w_plus_a * u_a_high + w_minus_a * u_a_low
    
    u_b_high = u(lottery["B"]["high"], lambda_)
    u_b_low = u(lottery["B"]["low"], lambda_)
    
    w_plus_b = lottery["B"]["prob_high"]
    w_minus_b = 1 - lottery["B"]["prob_high"]
    
    eu_b = w_plus_b * u_b_high + w_minus_b * u_b_low
   # print("eu_a:", eu_a, "eu_b:", eu_b, "lambda:", lambda_,"overconfidence:", overconfidence, "weight_loss:", weight_loss_)

    # ADDING NOISE TO THE DECISION
    error = np.random.normal(0, 0.5)
    choice = "A" if eu_a + error >= eu_b  else "B"
    
    return {
        "lottery": lottery_num,
        "lambda": lambda_,
        "eu_a": eu_a,
        "eu_b": eu_b,
        "choice": choice,
        "weight_loss": weight_loss_
    }


def how_much_to_send_prior(amount: float, lambda_: float, alpha: float, l: float = 0, rho: float = 0, f=lambda x: x**r_paramater, phi: float = 0, weight_loss: float = 1, weight_betrayal: float = 0.33, beta: float = None , beliefsAI: float = 0, treatment: str = "") -> int:
  """
  Determines how much to send in a trust game based on loss aversion and inequality aversion.
  m defines which lottery is played (m=1 plays lottery 1, m=2 plays lottery 2, etc).
  
  Args:
      amount: Initial endowment (c parameter)
      lambda_: Loss aversion parameter
      alpha: Inequality aversion parameter (higher alpha = more willing to send)
      phi: Aversion to negative outcomes (activated when outcome is negative)
      l: Prior belief about agent's likelihood to choose option A
      beta: Parameter for inequality aversion in betrayal case
      weight_loss: Weight for losses in prospect theory default to 1
      weight_betrayal: Weight for betrayal case in principal's utility, default to 1 out of 3 (possible choices)
      beliefsAI: Belief about LLM performance
      treatment: Treatment group (e.g., "LLM-advice" or "Control")

  Returns:
      Optimal amount to send (m, integer 1-10)
  """

  def u(x, lambda_val):
      """Utility function with loss aversion"""
      if x >= 0:
          return f(x)
      else:
        
    # The only way to create some variability in the amount sent was to multiply the utility function by a constant. 
          return - lambda_val * f(abs(x))*4
  
  def utility_for_lottery(m):
      """Calculate expected utility for sending m (which activates lottery m)"""
      
      if m == 0:
        return u(amount - alpha * max(amount + rho, 0), lambda_)
      
      if m < 1 or m > 10:
          return float('-inf')
      
      lottery = LOTTERY_CHOICES[m]
      # Principal evaluates expected utility based on prior probability that agent chooses A
      p_agent_chooses_a = l 
          
      # Expected utility when agent chooses A (gets outcome from A)
      p_a_high = lottery["A"]["prob_high"]
      p_a_low = (1 - p_a_high)
      eu_if_a = p_a_high * u(amount - m + lottery["A"]["high"] - alpha * max(amount - m + rho, 0), lambda_) + \
             p_a_low * weight_loss * u(amount - m + lottery["A"]["low"] - alpha * max(amount - m + rho, 0) - phi * rho, lambda_)
      
      # Expected utility when agent chooses B (gets outcome from B)
      p_b_high = lottery["B"]["prob_high"]
      p_b_low = 1 - p_b_high
      eu_if_b = p_b_high * u(amount - m + lottery["B"]["high"] - alpha*max(amount - m + rho,0), lambda_) + \
                p_b_low * u(amount - m + lottery["B"]["low"]  - alpha*max(amount - m + rho,0), lambda_)
                
     # Betrayal case: agent chooses to betray principal by keeping m (This is the only case where principal is inequality averse for having less than the agent)
     
      eu_if_betrayal = u(amount - m - beta * min(amount - m, m), lambda_)
                
      # Principal's expected utility for lottery m:
      # weighted by prior probability of agent's choice
     # print( p_agent_chooses_a * eu_if_a + (1 - p_agent_chooses_a) * eu_if_b + weight_betrayal * eu_if_betrayal)
     # print("eu_if_betrayal:", eu_if_betrayal)
      return p_agent_chooses_a * eu_if_a + (1 - p_agent_chooses_a) * eu_if_b +  weight_betrayal * eu_if_betrayal
  
  # ADDING NOISE TO THE DECISION OF HOW MUCH TO SEND
  error = int(np.random.normal(0, 0.5))
  utilities = {m: utility_for_lottery(m) for m in range(0, 11)}
  optimal_m = max(utilities, key=utilities.get) + error
  #print("M:", optimal_m , "lambda", lambda_, "alpha:", alpha, "beta:", beta, "beliefsAI:", beliefsAI, "treatment:", treatment)
  if treatment == "LLM-advice":
      if beliefsAI > 0:  # If the principal has a positive belief about LLM performance, they might be more willing to send more (activate higher lotteries)
          optimal_m = optimal_m + int(beliefsAI)  # If the principal has a strong belief that LLM performs well, they might be more willing to send more (activate higher lotteries)
  #       print("newOptimal_m:", optimal_m)
  return max(0, min(10, optimal_m))


def decide_over_punishment(amount: float, lambda_: float, alpha: float, l: float = 0, rho: float = 0, f=lambda x: x**0.8, phi: float = 0, weight_loss: float = 1, weight_betrayal: float = 0.33, beta: float = None, r_paramater: float = 0.88, m = None, lottery_chosen = None, choice_drawn = None, accountability = 1) -> float:
    """
    Decide how much to punish the agent based on amount sent and returned.
    Args:
        amount_sent: Amount sent by principal
        amount_returned: Amount returned by agent
        k: Sensitivity to unfairness
        h: Baseline punishment level
        
    """

    def u(x, lambda_val):
        """Utility function with loss aversion"""
        if x >= 0:
            return f(x)
        else:
            return -lambda_val * f(abs(x))
    
    def utility_for_lottery(rho):
        """Calculate expected utility for sending m (which activates lottery m)"""
        
        if m == 0:
            return u(amount - accountability*beta * max(amount + accountability*rho, 0), lambda_)
        
        if m >= 1 and m <= 10:
            lottery = LOTTERY_CHOICES[m]
            if choice_drawn == "high":
                return u(amount - m + lottery[lottery_chosen][choice_drawn] - accountability*alpha * max(amount - m - rho, 0), lambda_)
            elif choice_drawn == "low":
                return u(amount - m + lottery[lottery_chosen][choice_drawn] - accountability*alpha * max(amount - m - rho, 0) - phi*rho, lambda_)
        else:
            return float('-inf')
    
    utilities = {}
    for rho_val in np.linspace(-3, 3, 20):
        utilities[rho_val] = utility_for_lottery(rho_val)
       # print("rho_val:", rho_val, "utility:", utilities[rho_val])
    
    # If all utilities are equal, choose 0
    if len(set(utilities.values())) == 1:
        optimal_rho = 0
    else:
        optimal_rho = max(utilities, key=utilities.get)
        
    final_rho = optimal_rho + np.random.normal(0, 1)
    
    return max(-3, min(3, final_rho))



class Agent:
    """Represents an agent participant in the lottery game"""
    
    def __init__(self, agent_id: int, lambda_: float = None, type_dm: str = None, toInvest: str = None, treatment: str = "baseline", overconfidence: float = 0, weight_loss: float = 6 ):
        self.agent_id = agent_id
        self.type_dm = type_dm
        self.toInvest = toInvest
        self.treatment = treatment
        self.choices = []
        self.overconfidence = overconfidence
        self.weight_loss = weight_loss
        self.lambda_ = np.random.lognormal(mean=0.17, sigma=np.sqrt(0.29))
        self.weight_loss = 2                                # strong overweighting of losses
        
    def make_choice(self, lottery_num: int) -> str:
        """Make a lottery choice and record it"""
        result = evaluate_lottery_choice(lottery_num, lambda_=self.lambda_, weight_loss_=self.weight_loss)
        self.choices.append({
            "participant_id": f"{self.agent_id}",
            "role": "Agent",
            "lottery": lottery_num,
            "choice": result["choice"],
            "lambda": result["lambda"],
            "weight_loss": result["weight_loss"]
        })
        return result["choice"]
    
    def make_choice_trust_lottery(self, lottery_num: int) -> str:
        """Make a lottery choice after observing the amount sent record it"""
        
        if self.treatment == "LLM-advice":
            if self.type_dm == "Loss Avoider":
                self.overconfidence = np.random.uniform(0.3, 0.6) # less loss averse due to overconfidence
            elif self.type_dm == "Expected Utility Maximizer":
                self.overconfidence = 1  # less loss averse due to overconfidence  
            elif self.type_dm == "Payoff Sensitive/Non Consistent":
                self.overconfidence = np.random.uniform(0.1, 0.3) # moderate loss aversion due to overconfidence 
        elif self.treatment == "baseline":
            self.overconfidence = 1  # no overconfidence in baseline
               
        result = evaluate_lottery_choice_trust_over_risk(lottery_num, lambda_=self.lambda_ , overconfidence=self.overconfidence, weight_loss_=self.weight_loss)
        self.choices.append({
            "participant_id": f"{self.agent_id}",
            "role": "Agent",
            "lottery_played": lottery_num,
            "lambda": result["lambda"],
            "overconfidence": self.overconfidence,
            "weight_loss": result["weight_loss"]
        })
        return result["choice"]
    
    def determining_type_dm(self) -> str:
        """Classify based on the lottery choices made"""
        count_a = sum(1 for choice in self.choices if choice["choice"] == "A")
        count_b = sum(1 for choice in self.choices if choice["choice"] == "B")
        total_choices = len(self.choices)
        
        if count_a == total_choices:
            self.type_dm = "Expected Utility Maximizer"
        elif count_b == total_choices:
            self.type_dm = "Loss Avoider"
        else:
            self.type_dm = "Payoff Sensitive/Non Consistent"
        
        
    def get_choices_dataframe(self) -> pd.DataFrame:
        """Return choices as DataFrame"""
        df = pd.DataFrame(self.choices)
        df['treatment'] = self.treatment
        df['type_dm'] = self.type_dm
        df['toInvest'] = self.toInvest
        return df




 
class Principal:
    """Represents a principal participant in the lottery game"""
    
    def __init__(self, principal_id: int, amount: float = 10, treatment: str = "baseline", choice_drawn: str = None):
        self.principal_id = principal_id
        self.treatment = treatment
        self.amount = amount
        self.accountability = 1  # default accountability
        self.choice_drawn = choice_drawn if choice_drawn is not None else "default"  # Initialize choice_drawn
        self.payoff_lottery = 0  # Initialize payoff_lottery
        self.phi = 1
        self.beliefsAI = 0   # We assume that people has beliefs about LLM performance. Difference of the two estimations.
        self.alpha = np.random.normal(loc=0.425, scale=0.3) # estimates for the inequality aversion parameters                                    # baseline inequality aversion
        self.lambda_ = np.random.lognormal(mean=0.17, sigma=np.sqrt(0.29)) # Estimation taken from Pol campos pre-registration
        self.l = 0.75                                        # pessimistic belief
        self.weight_betrayal = 0.33                          # overweight betrayal
        self.weight_loss = 5                               # strong overweighting of losses
        self.beta = np.random.normal(loc=0.291, scale=0.3)       # strong inequality aversion in betrayal case 
        self.beliefsAI = np.random.normal(loc=2, scale=2)     # beliefs about LLM performance
        self.sent_amount = 0
        self.choices = []
    
    def make_choice(self, lottery_num: int) -> str:
        """Make a lottery choice and record it"""
        result = evaluate_lottery_choice(lottery_num, lambda_=self.lambda_)
        self.choices.append({
            "participant_id": f"{self.principal_id}",
            "role": "Principal",
            "lottery": lottery_num,
            "choice": result["choice"],
            "lambda": result["lambda"],
            "weight_loss_lottery": result["weight_loss"],
        })
        return result["choice"]
        
    def make_choice_how_much_to_send(self) -> int:
        """Decide how much to send in trust game"""
        m = how_much_to_send_prior(amount=self.amount, lambda_=self.lambda_, alpha=self.alpha, l=self.l, rho=0, phi=0, weight_betrayal=self.weight_betrayal, weight_loss=self.weight_loss, beta=self.beta, treatment=self.treatment, beliefsAI=self.beliefsAI)
        self.sent_amount = m
        return m
    
    def make_choice_how_much_to_punish(self, lottery_selected) -> float:
        if self.treatment == "LLM-advice":
            accountability = np.random.binomial(1, 0.1) # less accountability due to LLM advice
            self.accountability = accountability
        elif self.treatment == "baseline":
            accountability = 1  # full accountability in baseline
            self.accountability = accountability
            
        """Decide how much to punish or reward the agent"""
        rho = decide_over_punishment(amount=self.amount, lambda_=self.lambda_, phi=self.phi, alpha=self.alpha, l=self.l, weight_betrayal=self.weight_betrayal, weight_loss=self.weight_loss, beta=self.beta, m=self.sent_amount, choice_drawn=self.choice_drawn, lottery_chosen=lottery_selected, accountability=self.accountability)
        self.rho = rho
        return rho
    
    def get_choices_dataframe(self) -> pd.DataFrame:
        """Return choices as DataFrame"""
        df = pd.DataFrame(self.choices)
        df['sent'] = self.sent_amount
        df['amount'] = self.amount
        df['prior_prob_a'] = self.l
        df['alpha'] = self.alpha
        df['treatment'] = self.treatment
        df['weight_betrayal'] = self.weight_betrayal
        df['weight_loss'] = self.weight_loss
        df['beta'] = self.beta
        df['rho'] = self.rho
        df['choice_drawn'] = self.choice_drawn
        df['accountability'] = self.accountability
        df['payoff_lottery'] = self.payoff_lottery
        df['beliefsAI'] = self.beliefsAI
        return df
    
    
class MainExperimentSimulation:
    """Manages lottery simulation with agents and principals"""
    
    def __init__(self, num_agents: int, num_principals: int, num_observations: int = 300):
        self.num_agents = num_agents
        self.num_principals = num_principals
        self.num_observations = num_observations
        self.agents = [Agent(i, treatment=random.choice(["baseline", "LLM-advice"])) for i in range(1, num_agents + 1)]
        self.principals = [Principal(i, treatment=random.choice(["baseline", "LLM-advice"])) for i in range(1, num_principals + 1)]
        self.all_results = []
    
    def run_simulation(self):
        """Run simulation for all participants across all lotteries"""
        for _ in range(self.num_observations):
            for lottery_num in range(1, 11): 
                for agent in self.agents:
                   # print(f"Agent {agent.agent_id} making choice for lottery {lottery_num}")
                    agent.make_choice(lottery_num)
                    agent.determining_type_dm()
                
                for principal in self.principals:
                   # print(f"Principal {principal.principal_id} making choice for lottery {lottery_num}")
                    principal.make_choice(lottery_num)
            
            for principal in self.principals:
                principal.make_choice_how_much_to_send()
               # print(f"Principal {principal.principal_id} sent: {principal.sent_amount}")
                for agent in self.agents:
                    if agent.agent_id == principal.principal_id:
                        agent.determining_type_dm()
                        if principal.sent_amount == 0:
                            agent_choice = agent.make_choice_trust_lottery(principal.sent_amount)
                            principal.make_choice_how_much_to_punish(lottery_selected = None)
                           # print(f"Principal {principal.principal_id} decided punishment/reward rho: {principal.rho}")

                            
                        else:
                            agent_choice = agent.make_choice_trust_lottery(principal.sent_amount)
                            agent.toInvest = agent_choice
                           # print(f"Agent {agent.agent_id} chose: {agent_choice}")
                            if principal.principal_id == agent.agent_id:
                                principal.choice_drawn = np.random.choice(["high", "low"], p=[LOTTERY_CHOICES[principal.sent_amount][agent_choice]["prob_high"], 1 - LOTTERY_CHOICES[principal.sent_amount][agent_choice]["prob_high"]])
                                principal.payoff_lottery = LOTTERY_CHOICES[principal.sent_amount][agent_choice][principal.choice_drawn]
                               # print(f"Principal {principal.principal_id} observed outcome: {principal.choice_drawn}")
                               # print(f"Principal {principal.principal_id} alpha: {principal.alpha}")
                               # print(f"Principal {principal.principal_id} Treatment: {principal.treatment}")
                                principal.make_choice_how_much_to_punish(lottery_selected = agent_choice)
                               # print(f"Principal {principal.principal_id} decided punishment/reward rho: {principal.rho}")
                         
        
    
    def compile_results(self) -> pd.DataFrame:
        """Compile all results into a single DataFrame"""
        for agent in self.agents:
            self.all_results.extend(agent.get_choices_dataframe().to_dict('records'))
        
        for principal in self.principals:
            self.all_results.extend(principal.get_choices_dataframe().to_dict('records'))
        
        return pd.DataFrame(self.all_results)
    
    def save_to_csv(self, filename: str = "lottery_simulation_results.csv"):
        """Save results to CSV file"""
        df = self.compile_results()
        df.to_csv(filename, index=False)
       # print(f"Results saved to {filename}")
        return df


if __name__ == "__main__":
    observations = 300  # Number of times each participant goes through the lottery choices
    simulation = MainExperimentSimulation(num_agents= int(observations/2), num_principals= int(observations/2), num_observations=1)
    simulation.run_simulation()
    results_df = simulation.save_to_csv()
   # print(results_df.head(20))
    print("Simulation completed")

