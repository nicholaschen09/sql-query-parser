# Use official Node.js 20 image
FROM node:20-alpine

# Set working directory
WORKDIR /app

# Copy package files and install dependencies
COPY package*.json ./
RUN npm install

# Copy the rest of the app
COPY . .

# Build the frontend
RUN npm run build

# Expose port (adjust if your backend uses a different port)
EXPOSE 3000

# Start the backend server (adjust if your entry point is different)
CMD ["npm", "run", "server", "--", "sample-data.json"] 