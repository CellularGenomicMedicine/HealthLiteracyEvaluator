# Author: Ping Cao 
# https://github.com/creatorcao
"""
Use LLM to simplify medical text and evaluate the readability scors. 
"""
import os, re
import pandas as pd
import streamlit as st
import matplotlib.pyplot as plt

from io import StringIO
from langchain.chains import LLMChain
from langchain.prompts import PromptTemplate
from langchain_openai import ChatOpenAI
from dotenv import load_dotenv
load_dotenv()

template = """ 
You are an expert assistant skilled in simplifying complex medical and genetic information for diverse audiences. Your task is to:
1. Rewrite the TEXT for individuals with low literacy. Use clear, everyday language and avoid technical jargon, the **original text** after simplification is **simplified text**.
2. Calculate the evaluation scores for both the **original text** and the **simplified text** using the following formulas:  

   **Flesch Reading-Ease (FRE)**:  
   FRE = 206.835 - 1.015 × (number of words) / (number of sentences) - 84.6 × (number of syllables) / (number of words)
   
   **Gunning Fog Index (GFI)**: 
   GFI = 0.4 x (number of words) / (number of sentences) + 100 x (number of complex words) / (number of words)

   **Flesch-Kincaid Grade Level (FKGL)**: 
   FKGL = 0.39 x (number of words) / (number of sentences) + 11.8 x (number of syllables) / (number of words) - 15.59

   **Coleman-Liau Index (CLI)**
   CLI = 5.89 x (number of characters) / (number of words) - 0.3 x (number of sentences) / (number of words) - 15.8

   **Automatic Readability Index (ARI)**
   ARI = 4.71 x (number of characters) / (number of words) + 0.5 x (number of words) / (number of sentences) - 21.43
   
   **Simple Measure of Gobbledygook (SMOG)**:  
   SMOG = 1.043 × sqrt((number of words more than 3 syllables) × 30 / (number of sentences)) + 3.1291  

   **Linsear Write Formula (LWF)**:  
   LWF = (100-100x(number of words less than 3 syllables)/(number of words) + 3x100x(number of words more than 3 syllables)/(number of words)) / (100*(number of sentences)/(number of words))

Formatting Requirements:
1. Provide the **simplified text**, formatted as `### Simplified Text`.
2. Output the scores as CSV. Use this format:
```
Metric,Original Score,Simplified Score
FRE,value1,value2
GFI,value1,value2
FKGL,value1,value2
CLI,value1,value2
ARI,value1,value2
SMOG,value1,value2
LWF,value1,value2

```
   
Here is the TEXT:
{text}

Your response should include:
1. The **simplified text**, formatted as `### Simplified Text`.
2. The scores formatted as CSV.

"""

def map_readability_scores(df):
    def get_grade_level(metric, score):
        if metric == 'GFI':
            if 6 <= score < 7:
                return '6'
            elif 7 <= score < 8:
                return '7'
            elif 8 <= score < 9:
                return '8'
            elif 9 <= score <= 12:
                return '9-12'
            elif 13 <= score <= 15:
                return '13-15'
            elif 15 < score:
                return '16-18'
            
        elif metric == 'FKGL':
            if 0 <= score < 6:
                return '5'
            elif 6 <= score < 7:
                return '6'            
            elif 7 <= score < 8:
                return '7'
            elif 8 <= score < 9:
                return '8'
            elif 9 <= score < 13:
                return '9-12'
            elif 13 <= score <= 15:
                return '13-15'
            elif score > 15:
                return '16-18'
            
        elif metric == 'CLI':
            if 0 <= score <= 6:
                return '5'   
            elif 6 <= score < 7:
                return '6'            
            elif 7 <= score < 8:
                return '7'
            elif 8 <= score < 9:
                return '8'
            elif 9 <= score <= 11:
                return '9-12'
            elif 11 < score <= 12:
                return '13-15'
            elif 12 < score:
                return '16-18'
            
        elif metric == 'ARI':
            if 0 <= score < 6:
                return '5'
            elif 6 <= score < 7:
                return '6'            
            elif 7 <= score < 8:
                return '7'
            elif 8 <= score < 9:
                return '8'
            elif 9 <= score < 11:
                return '9-12'
            elif 11 <= score <= 12:
                return '13-15'
            elif score > 12:
                return '16-18'
            
        elif metric == 'SMOG':
            if 0 <= score < 7:
                return '5'   
            elif 7 <= score < 8:
                return '6'            
            elif 8 <= score < 9:
                return '7'
            elif 9 <= score < 10:
                return '8'         
            elif 10 <= score <= 12:
                return '9-12'
            elif 12 < score <= 18:
                return '13-15'
            elif 18 < score:
                return '16-18'
            
        elif metric == 'LWF':
            if 0 <= score <= 6:
                return '5'
            elif 6 < score <= 7:
                return '6'
            elif 7 < score <= 8:
                return '7'            
            elif 8 < score < 9:
                return '8'            
            elif 9 <= score <= 10:
                return '9-12'                
            elif 11 <= score <= 12:
                return '13-15'
            elif score > 12:
                return '16-18'
            
        elif metric == 'FRE':
            if 90.0 <= score <= 100.0:
                return '5'
            elif 80.0 <= score < 90.0:
                return '6'
            elif 70.0 <= score < 80.0:
                return '7'
            elif 65.0 <= score < 70.0:
                return '8'            
            elif 50.0 <= score < 65.0:
                return '9-12'
            elif 30.0 <= score < 50.0:
                return '13-15'
            elif score < 30.0:
                return '16-18'
            
        return 'Unknown'

    df['Original Grade'] = df.apply(lambda row: get_grade_level(row['Metric'], row['Original Score']), axis=1)
    df['Simplified Grade'] = df.apply(lambda row: get_grade_level(row['Metric'], row['Simplified Score']), axis=1)
    
    df['Original Grade'] = df['Original Grade'].map(grade_mapping)
    df['Simplified Grade'] = df['Simplified Grade'].map(grade_mapping)

    df['Original Grade'] = pd.Categorical(df['Original Grade'], categories=grade_order, ordered=True)
    df['Simplified Grade'] = pd.Categorical(df['Simplified Grade'], categories=grade_order, ordered=True)
  
    return df

grade_mapping = {
    '5': "Elementary School",
    '6': "Middle School",
    '7': "Middle School",
    '8': "Middle School",
    '9-12': "High School",
    '13-15': "College",
    '16-18': "Professor"
}

grade_order = ["Elementary School", 
               "Middle School", 
               "High School", 
               "College", 
               "Professor"
              ]


prompt = PromptTemplate(input_variables=["text"], template=template)
llm = ChatOpenAI(temperature=0, model="gpt-4-turbo")
chain = LLMChain(llm=llm, prompt=prompt)

def generate_response(message):     
    result = chain.invoke({"text": message})['text']
    simplified_text_match = re.search(r'### Simplified Text\n(.*?)(?=\n\w+,Original)', result, re.DOTALL)
    simplified_text = simplified_text_match.group(1).strip().rstrip('`\n')
    csv_match = re.search(r'\w+,Original Score,Simplified Score\n(.*)', result, re.DOTALL)
    csv_data = csv_match.group(0).strip().rstrip('`\n')
    df = pd.read_csv(StringIO(csv_data))
    df = map_readability_scores(df)

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8, 10))
    x = range(len(df))
    width = 0.35
    # grade plot
    original_bars = ax1.bar([i - width/2 for i in x], df['Original Grade'].cat.codes, width, label='Original Text', 
                           color='#bc80bd', alpha=0.6)
    for i, bar in enumerate(original_bars):
        height = bar.get_height()

    simplified_bars = ax1.bar([i + width/2 for i in x], df['Simplified Grade'].cat.codes, width, label='Simplified Text', 
                             color='#80b1d3', alpha=0.9)
    for i, bar in enumerate(simplified_bars):
        height = bar.get_height()

    ax1.set_xlabel('Readability Metrics', fontsize = 10)
    ax1.set_ylabel('Grade Level', fontsize=10)
    ax1.set_title('Readability Grade', weight='bold', fontsize=12)
    ax1.set_xticks([i for i in x])
    ax1.set_xticklabels(df['Metric'], ha='right')
    ax1.set_yticks(range(len(grade_order))) 
    ax1.set_yticklabels(grade_order) 
    ax1.spines['top'].set_visible(False)
    ax1.spines['right'].set_visible(False)
    # score plot    
    original_bars = ax2.bar([i - width/2 for i in x], df['Original Score'], width, label='Original text', 
                           color='#bc80bd', alpha=0.6)
    for i, bar in enumerate(original_bars):
        height = bar.get_height()
        ax2.text(bar.get_x() + bar.get_width()/2., height,
                f'{df["Original Score"][i]:.1f}',
                ha='center', va='bottom', fontsize=10)
        
    simplified_bars = ax2.bar([i + width/2 for i in x], df['Simplified Score'], width, label='Simplified text', 
                             color='#80b1d3', alpha=0.9)
    for i, bar in enumerate(simplified_bars):
        height = bar.get_height()
        ax2.text(bar.get_x() + bar.get_width()/2., height,
                f'{df["Simplified Score"][i]:.1f}',
                ha='center', va='bottom', fontsize=10)
    
    ax2.set_xlabel('Readability Metrics', fontsize = 10)
    ax2.set_ylabel('Score', fontsize = 10)
    ax2.set_title('Readability Scores', weight='bold', fontsize = 12)
    ax2.set_xticks([i for i in x])
    ax2.set_xticklabels(df['Metric'])
    ax2.spines['top'].set_visible(False)
    ax2.spines['right'].set_visible(False)
    ax1.legend(loc=1)
    plt.tight_layout()
    plt.subplots_adjust(hspace=0.5)
    plot_filename = 'readability_scores.png'
    plt.savefig(plot_filename)
    plt.close()
    
    return simplified_text, plot_filename  

def main():
    with st.sidebar:
        st.title("💬 Welcome to Gensimbot")       
        st.write("This app aims to simplify text and evaluate readability scores. Developed by [Ping Cao](https://creatorcao.github.io), view the source code at [GitHub](https://github.com/CellularGenomicMedicine).")
        st.write("Flesch Reading-Ease (FRE)\n\nGunning Fog Index (GFI)\n\nFlesch-Kincaid Grade Level (FKGL)\n\nColeman-Liau Index (CLI)\n\nAutomatic Readability Index (ARI)\n\nSimple Measure of Gobbledygook (SMOG)\n\nLinsear Write Formula (LWF)")
    
    st.session_state.setdefault("messages", [{"role": "assistant", "content": "How can I help you?"}])

    for msg in st.session_state["messages"]:
        if "image" in msg:
            st.chat_message(msg["role"]).image(msg["image"])
        else:
            st.chat_message(msg["role"]).write(msg["content"])
    
    if user_input := st.chat_input():
        st.session_state["messages"].append({"role": "user", "content": user_input})
        st.chat_message("user").write(user_input)

        simplified_text, plot_filename = generate_response(user_input)

        st.session_state["messages"].append({
                "role": "assistant",
                "content": simplified_text,
                "image": plot_filename
            })
        
        chat_msg = st.chat_message("assistant")
        chat_msg.write(f"**Simplified text**:\n\n{simplified_text}\n\n")
        chat_msg.image(plot_filename)

if __name__ == '__main__':
    main()
