# 🎲 Gacha Pull Logger (R Shiny)

A **fan-made gacha pull logging and analytics web application** built using **R and Shiny**.  

This project allows users to record pulls, analyze pity trends, and manage pull data using CRUD operations.

---

## 📸 Demo

View the live demo here:  
[Open Live Demo](https://gacha-pull-logger.onrender.com)

Username: Kurt013  
Password: @GenshinImpact13

![Login Page UI](https://raw.githubusercontent.com/Kurt013/gacha-pull-logger/refs/heads/main/www/assets/login-page.png)

![Logger Page UI](https://raw.githubusercontent.com/Kurt013/gacha-pull-logger/refs/heads/main/www/assets/logger-page.png)

![Analytics Page UI](https://raw.githubusercontent.com/Kurt013/gacha-pull-logger/refs/heads/main/www/assets/analytics-page.png)

![Import Wish History Modal UI](https://raw.githubusercontent.com/Kurt013/gacha-pull-logger/refs/heads/main/www/assets/import-page.png)

---

## 📦 Installation

1. **Clone the repository**:

```bash
git clone https://github.com/Kurt013/gacha-pull-logger.git
cd gacha-pull-logger
```

2. **Build Docker image**:

```bash
docker build -t gacha-pull-logger .
```

3. **Run the app in a container**:

```bash
docker run -p 3838:3838 gacha-pull-logger
```

4. **Access the app**:
  Open your browser and go to:

```bash
http://localhost:3838
```

---

## ✨ Features
- User authentication and session management
- Gacha pull CRUD operations
- Automatic pity calculation and tracking
- Excel bulk import with preview
- Analytics dashboard (luck index, trends, statistics)
- Real-time pity visualization with soft/hard pity markers
- Multi-banner support (Character, Weapon, Standard)
- **Genshin API Integration** - Fetch character/weapon details (images, descriptions, stats) via [Genshin.jmp.blue API](https://genshin.jmp.blue)
- Dockerized deployment

---

## 🧰 Tech Stack

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white&style=for-the-badge)
![Shiny](https://img.shields.io/badge/Shiny-FF2D20?logo=r&logoColor=white&style=for-the-badge)
![SQLite](https://img.shields.io/badge/SQLite-003B57?logo=sqlite&logoColor=white&style=for-the-badge)
![Docker](https://img.shields.io/badge/Docker-2496ED?logo=docker&logoColor=white&style=for-the-badge)
![HTML5](https://img.shields.io/badge/HTML5-E34F26?logo=html5&logoColor=white&style=for-the-badge)
![CSS3](https://img.shields.io/badge/CSS3-1572B6?logo=css3&logoColor=white&style=for-the-badge)
![JavaScript](https://img.shields.io/badge/JavaScript-F7DF1E?logo=javascript&logoColor=000&style=for-the-badge)

---

## 🎨 Design Prototype

View the full Figma design here:  
[Open Figma](https://www.figma.com/design/pVGyrisy56GvcW9UbcH0in/Gacha-Pull-Logger--Genshin-Themed-?node-id=0-1&m=dev)

---

## ⚠️ Disclaimer

This project is **not affiliated with, endorsed by, or sponsored by HoYoverse.**
All game-related names, terms, and concepts referenced are used solely for **descriptive and educational purposes.**

---

## 📄 License

See [LICENSE](LICENSE) for full details.  