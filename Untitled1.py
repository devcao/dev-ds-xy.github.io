#!/usr/bin/env python
# coding: utf-8

# In[20]:


import pandas as pd
import numpy as np


# In[2]:


data = pd.read_csv("311_Service_Requests_from_2010_to_Present.csv")


# In[3]:


data.head()


# In[4]:


data.shape


# In[9]:


small_data = data[0:5000]


# In[12]:


small_data.describe()


# In[45]:


import datetime
print(datetime.datetime.now())


# In[49]:


pd.to_datetime(small_data["Created Date"][0]).year


# In[ ]:





# In[56]:


small_data["Created Date"][0:10]


# In[ ]:


dd = data["Created Date"].apply(lambda x: pd.to_datetime(x, format="%m/%d/%Y %I:%M:%S %p"))


# In[ ]:


dd[0:10]


# In[51]:


small_data["Created Date"].apply(lambda x: x.date())


# In[31]:


import geopy.distance

coords_1 = np.array( [52.2296756, 21.0122287] )
coords_2 = np.array( [52.406374, 16.9251681] )

print(geopy.distance.vincenty(coords_1, coords_2).km)
print(np.square(coords_1 - coords_2).sum())

coords_1 = np.array( [51.2296756, 21.0122287] )
coords_2 = np.array( [51.406374, 16.9251681] )

print(geopy.distance.vincenty(coords_1, coords_2).km)
print(np.square(coords_1 - coords_2).sum())


# In[29]:


np.square(coords_1 - coords_2).sum()


# In[ ]:


data.describe()

