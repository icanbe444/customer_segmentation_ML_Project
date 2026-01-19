Customer Segmentation Using Clustering Algorithms

This project explores customer segmentation using unsupervised machine learning. The goal is simple: group customers based on similarities in demographics and spending behavior, and make the results easy to understand—even for non-technical users.

What This Project Does

Uses K-Means and DBSCAN to segment existing customers.

Works with demographic data and spending scores.

Compares performance and limitations of both algorithms.

Provides a simple web application to visualize and analyze clustering results.

Key Findings

K-Means

Optimal number of clusters determined using the Elbow Method.

Produced 6 distinct customer groups.

Silhouette score: 0.24.

Limitation: poor handling of noise and outliers.

DBSCAN

Identified 218 clusters.

Effectively handled noise by isolating 2,530 out of 6,718 data points.

Silhouette score: 0.317, outperforming K-Means.

Better suited for datasets with irregular cluster shapes and noise.

Why This Matters

Customer segmentation helps businesses:

Understand customer behavior.

Design targeted marketing strategies.

Improve customer engagement and retention.

This project lowers the barrier to entry by allowing users to run and explore clustering results without deep data analytics knowledge.

Tech Stack

Python

R

Visualization libraries

Lightweight web framework for result exploration

Future Improvements

Clearer labeling and interpretation of customer groups.

Performance and usability optimizations in the web application.

Support for additional clustering techniques.
