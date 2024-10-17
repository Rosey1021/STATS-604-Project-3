const express = require('express');
const bodyParser = require('body-parser');
const fs = require('fs');
const path = require('path');
const app = express();
const PORT = 3000;

// Serve static files from the "public" directory
app.use(express.static(path.join(__dirname, 'public')));

// Body parser to handle JSON requests
app.use(bodyParser.json());

// Route to handle saving the ratings
app.post('/save-ratings', (req, res) => {
    const ratings = req.body;

    // Define the CSV header and content
    const csvHeader = 'BananaID,Rating\n';
    const csvContent = ratings.map(r => `${r.bananaID},${r.rating}`).join('\n');
    const csvData = csvHeader + csvContent;

    // Save to a CSV file
    fs.writeFile(path.join(__dirname, 'banana_ratings.csv'), csvData, (err) => {
        if (err) {
            return res.status(500).json({ message: 'Error saving ratings' });
        }

        res.json({ message: 'Ratings saved successfully!' });
    });
});

// Start the server
app.listen(PORT, () => {
    console.log(`Server running on http://localhost:${PORT}`);
});
