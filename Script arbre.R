
library(rpart)
library(rpart.plot)

# 1. Importer le jeu de données
# Remplacez 'chemin/vers/votre_fichier.csv' par le chemin vers votre fichier
data <- read.csv("C:/Users/fb25d/Downloads/filtered_data.csv", header = TRUE)

# 2. Visualisation des premières lignes des données
head(data)

# 3. Suppression des colonnes inutiles
# Exclure des colonnes telles que 'X', 'index', 'tconst', et 'primaryTitle' qui ne sont pas informatives
data_clean <- subset(data, select = -c(X, index, tconst, primaryTitle))

# 4. Transformation des variables
# Convertir les colonnes de genres (Action, Adventure, etc.) et autres colonnes pertinentes en facteurs
cols_to_factor <- c("first", "second", "third", 
                    "Action", "Adventure", "Animation", "Biography", "Comedy", "Crime",
                    "Documentary", "Drama", "Family", "Fantasy", "History", "Horror", 
                    "Music", "Musical", "Mystery", "News", "Reality.TV", "Romance", 
                    "Sci.Fi", "Sport", "Thriller", "War", "Western", "groupe")

data_clean[cols_to_factor] <- lapply(data_clean[cols_to_factor], as.factor)

# 5. Construction de l'arbre de décision
# Spécification du modèle pour prédire 'averageRating' (note moyenne)
set.seed(123)  # Pour la reproductibilité
arbre_modele <- rpart(averageRating ~ ., 
                      data = data_clean, 
                      method = "anova",   # 'anova' pour la prédiction de valeurs numériques
                      control = rpart.control(minsplit = 10, cp = 0.01))  # Paramètres de contrôle

# 6. Visualisation de l'arbre de décision
rpart.plot(arbre_modele, 
           type = 3, 
           extra = 101, 
           fallen.leaves = TRUE, 
           main = "Arbre de Décision pour la Prédiction de la Note d'un Film")

# 7. Évaluation du modèle
# Prédiction des valeurs sur les données d'entraînement
predictions <- predict(arbre_modele, data_clean)

# Calcul de l'erreur quadratique moyenne (RMSE)
rmse <- sqrt(mean((data_clean$averageRating - predictions)^2))
cat("Erreur Quadratique Moyenne (RMSE) :", rmse)
