
# coding: utf-8

# In[1]:


import pandas as pd
from sqlalchemy.dialects.mssql.information_schema import columns
import numpy as np
import matplotlib.pyplot as plt
import os
from sklearn.decomposition import PCA as pca
from sklearn.decomposition import FactorAnalysis as fact
import sklearn.metrics as metcs
from scipy.cluster import hierarchy as hier
from sklearn import cluster as cls
from sklearn.feature_extraction.image import grid_to_graph
from sklearn import tree
from sklearn.externals.six import StringIO
from IPython.display import Image
import pydotplus
from scipy.stats.stats import pearsonr
from factor_analyzer import FactorAnalyzer
from sklearn import datasets
from sklearn.feature_selection import RFE
from sklearn.linear_model import LogisticRegression
import statsmodels.api as sm
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
from sklearn import model_selection
from sklearn.model_selection import cross_val_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_curve, auc


# In[2]:


os.chdir("D:\\varnika\\r and python\\project\\project")


# In[3]:


cleaned_data=pd.read_table('cleaned_data.csv', sep=',')


# In[4]:


cleaned_data['issue_d']=pd.to_datetime(cleaned_data['issue_d'])
cleaned_data['earliest_cr_line']=pd.to_datetime(cleaned_data['earliest_cr_line'])
cleaned_data['last_pymnt_d']=pd.to_datetime(cleaned_data['last_pymnt_d'])
cleaned_data['last_credit_pull_d']=pd.to_datetime(cleaned_data['last_credit_pull_d'])
cleaned_data['term'] = cleaned_data['term'].astype('category')
cleaned_data['grade'] = cleaned_data['grade'].astype('category')
cleaned_data['sub_grade'] = cleaned_data['sub_grade'].astype('category')
cleaned_data['emp_title'] = cleaned_data['emp_title'].astype('category')
cleaned_data['emp_length'] = cleaned_data['emp_length'].astype('category')
cleaned_data['home_ownership'] = cleaned_data['home_ownership'].astype('category')
cleaned_data['verification_status'] = cleaned_data['verification_status'].astype('category')
cleaned_data['purpose'] = cleaned_data['purpose'].astype('category')
cleaned_data['title'] = cleaned_data['title'].astype('category')
cleaned_data['delinq_2yrs'] = cleaned_data['delinq_2yrs'].astype('category')
cleaned_data['inq_last_6mths'] = cleaned_data['inq_last_6mths'].astype('category')


# In[5]:


cleaned_data_split_1 = cleaned_data[cleaned_data.loan_status==1]
splitnum = np.round((len(cleaned_data_split_1.index) * 0.6), 0).astype(int)
splitnum
cleaned_data_split_1_train = cleaned_data_split_1.sample(n=splitnum, replace=False)
len(cleaned_data_split_1_train.index)
cleaned_data_split_0 = cleaned_data[cleaned_data.loan_status==0]
splitnum1 = np.round((len(cleaned_data_split_0.index) * 0.6), 0).astype(int)
splitnum1
cleaned_data_split_0_train = cleaned_data_split_0.sample(n=splitnum1, replace=False)
len(cleaned_data_split_0_train.index)
cleaned_data_sample=cleaned_data_split_1_train.append([cleaned_data_split_0_train],ignore_index="True")


# In[6]:


cleaned_data_term_dummy = pd.get_dummies(cleaned_data_sample['term'], prefix='term')
cleaned_data_term_dummy.head()
cleaned_finaldata = cleaned_data_sample.join(cleaned_data_term_dummy)

cleaned_data_grade_dummy = pd.get_dummies(cleaned_data_sample['grade'], prefix='grade')
cleaned_data_grade_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_grade_dummy)

cleaned_data_subgrade_dummy = pd.get_dummies(cleaned_data_sample['sub_grade'], prefix='subgrade')
cleaned_data_subgrade_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_subgrade_dummy)

cleaned_data_emplength_dummy = pd.get_dummies(cleaned_data_sample['emp_length'], prefix='emplength')
cleaned_data_emplength_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_emplength_dummy)

cleaned_data_homeownership_dummy = pd.get_dummies(cleaned_data_sample['home_ownership'], prefix='homeownership')
cleaned_data_homeownership_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_homeownership_dummy)

cleaned_data_verificationstatus_dummy = pd.get_dummies(cleaned_data_sample['verification_status'], prefix='verificationstatus')
cleaned_data_verificationstatus_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_verificationstatus_dummy)

cleaned_data_purpose_dummy = pd.get_dummies(cleaned_data_sample['purpose'], prefix='purpose')
cleaned_data_purpose_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_purpose_dummy)

cleaned_data_title_dummy = pd.get_dummies(cleaned_data_sample['title'], prefix='title')
cleaned_data_title_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_title_dummy)

cleaned_data_delinq2yrs_dummy = pd.get_dummies(cleaned_data_sample['delinq_2yrs'], prefix='delinq2yrs')
cleaned_data_delinq2yrs_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_delinq2yrs_dummy)

cleaned_data_inqlast6mths_dummy = pd.get_dummies(cleaned_data_sample['inq_last_6mths'], prefix='inqlast6mths')
cleaned_data_inqlast6mths_dummy.head()
cleaned_finaldata = cleaned_finaldata.join(cleaned_data_inqlast6mths_dummy)

cleaned_finaldata.drop(['term','addr_state','grade','sub_grade','emp_title','emp_length','home_ownership','verification_status','purpose','title','delinq_2yrs','inq_last_6mths','issue_d','earliest_cr_line','last_pymnt_d','last_credit_pull_d','member_id'], axis=1, inplace=True)

cleaned_finaldata = cleaned_finaldata.rename(columns={'term_ 36 months': 'term_36_months', 'term_ 60 months': 'term_60_months'})
cleaned_finaldata = cleaned_finaldata.rename(columns={'emplength_1 yea': 'emplength_1_year', 'emplength_10+ years': 'emplength_10+_years', 'emplength_2 years': 'emplength_2_years', 'emplength_3 years': 'emplength_3_years', 'emplength_4 years': 'emplength_4_years', 'emplength_5 years': 'emplength_5_years', 'emplength_6 years': 'emplength_6_years', 'emplength_7 years': 'emplength_7_years', 'emplength_8 years': 'emplength_8_years', 'emplength_9 years': 'emplength_9_years', 'emplength_< 1 year': 'emplength_<_1_year'})
cleaned_finaldata = cleaned_finaldata.rename(columns={'title_Car financing': 'title_Car_financing', 'title_Credit Card Consolidation': 'title_Credit_Card_Consolidation', 'title_Credit card refinancing': 'title_Credit_card_refinancing', 'title_Debt consolidation': 'title_Debt_consolidation', 'title_Green loan': 'title_Green_loan', 'title_Home buying': 'title_Home_buying', 'title_Home improvement': 'title_Home_improvement', 'title_Major purchase': 'title_Major_purchase', 'title_Medical expenses': 'title_Medical_expenses', 'title_Moving and relocation': 'title_Moving_and_relocation'})
cleaned_finaldata = cleaned_finaldata.rename(columns={'verificationstatus_Not Verified': 'verificationstatus_Not_Verified'})
cleaned_finaldata = cleaned_finaldata.rename(columns={'inqlast6mths_0.0': 'inqlast6mths_0', 'inqlast6mths_1.0': 'inqlast6mths_1', 'inqlast6mths_2.0': 'inqlast6mths_2', 'inqlast6mths_3.0': 'inqlast6mths_3', 'inqlast6mths_4.0': 'inqlast6mths_4', 'inqlast6mths_5.0': 'inqlast6mths_5'})


# In[8]:


cleaned_finaldata.columns.values.tolist()


# In[37]:


decision_tree_data=cleaned_finaldata


# In[38]:


classnames = list(decision_tree_data.loan_status.unique())
inp1 = decision_tree_data.ix[:,[1,2,4,5,11,12,20,29,89,94,95,96,103,115,117,121]].dropna()

labels = inp1.ix[:, [2]]
inp1 = inp1.ix[:,[0,1,3,4,5,6,7,8,9,10,11,12,13,14,15]]


# In[46]:


dtree = tree.DecisionTreeClassifier(criterion = "gini", splitter = 'random', max_leaf_nodes = 5, min_samples_leaf = 5, max_depth= 5)
dtree.fit(inp1,labels)
dot_data = StringIO()


# In[47]:


indices = [0,1,3,4,5,6,7,8,9,10,11,12,13,14,15]


# In[48]:


tree.export_graphviz(dtree,
                     out_file=dot_data,
                     feature_names=list(inp1),
                     class_names=['paid','default'],
                     filled=True,
                     rounded=True,
                     special_characters=True)
graph = pydotplus.graph_from_dot_data(dot_data.getvalue())
Image(graph.create_png())


# In[61]:


X=decision_tree_data.ix[:,[1,2,5,11,12,20,29,89,94,95,96,103,115,117,121]]
y=decision_tree_data['loan_status']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)
dtree = tree.DecisionTreeClassifier(criterion = "gini", splitter = 'random', max_leaf_nodes = 5, min_samples_leaf = 5, max_depth= 5)
dtree.fit(X_train, y_train)
predicted = dtree.predict(X_test)
print('Accuracy of decision tree on test set: {:.2f}'.format(dtree.score(X_test, y_test)))


# In[62]:


print(metrics.classification_report(y_test, predicted))


# In[63]:


cm = metrics.confusion_matrix(y_test, predicted)
print(cm)

plt.matshow(cm)
plt.title('Confusion Matrix')
plt.xlabel('Actual Value')
plt.ylabel('Predicted Value')
plt.xticks([0,1], ['I','II'])


# In[64]:


from sklearn.metrics import roc_curve, auc
fpr, tpr,_=roc_curve(y_test, predicted,drop_intermediate=False)


# In[65]:


import matplotlib.pyplot as plt
plt.figure()
##Adding the ROC
plt.plot(fpr, tpr, color='red',
 lw=2, label='ROC curve')
##Random FPR and TPR
plt.plot([0, 1], [0, 1], color='blue', lw=2, linestyle='--')
##Title and label
plt.xlabel('FPR')
plt.ylabel('TPR')
plt.title('ROC curve')
plt.show()

