module Main

import Rhone.JS

--------------------------------------------------------------------------------
--          Model
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

main : IO ()
main = pure ()


--	<body>
--		<section id="todoapp_id" class="todoapp">
--			<header class="header">
--				<h1>todos</h1>
--				<input id="todo_input" class="new-todo" placeholder="What needs to be done?" autofocus>
--			</header>
--			<!-- This section should be hidden by default and shown when there are todos -->
--			<section id="main_section" class="main">
--				<input id="toggle-all" class="toggle-all" type="checkbox">
--				<label for="toggle-all">Mark all as complete</label>
--				<ul id="todolist_id" class="todo-list">
--					<!-- These are here just to show the structure of the list items -->
--					<!-- List items should get the class `editing` when editing and `completed` when marked as completed -->
--					<li class="completed">
--						<div class="view">
--							<input class="toggle" type="checkbox" checked>
--							<label>Taste JavaScript</label>
--							<button class="destroy"></button>
--						</div>
--						<input class="edit" value="Create a TodoMVC template">
--					</li>
--					<li>
--						<div class="view">
--							<input class="toggle" type="checkbox">
--							<label>Buy a unicorn</label>
--							<button class="destroy"></button>
--						</div>
--						<input class="edit" value="Rule the web">
--					</li>
--				</ul>
--			</section>
--			<!-- This footer should be hidden by default and shown when there are todos -->
--			<footer id="footer_id" class="footer">
--				<!-- This should be `0 items left` by default -->
--				<span class="todo-count"><strong>0</strong> item left</span>
--				<!-- Remove this if you don't implement routing -->
--				<ul class="filters">
--					<li>
--						<a class="selected" href="#/">All</a>
--					</li>
--					<li>
--						<a href="#/active">Active</a>
--					</li>
--					<li>
--						<a href="#/completed">Completed</a>
--					</li>
--				</ul>
--				<!-- Hidden if no completed items are left ↓ -->
--				<button id="clear_id" class="clear-completed">Clear completed</button>
--			</footer>
--		</section>
--		<footer class="info">
--			<p>Double-click to edit a todo</p>
--			<!-- Remove the below line ↓ -->
--			<p>Template by <a href="http://sindresorhus.com">Sindre Sorhus</a></p>
--			<p>Created by <a href="http://todomvc.com">Stefan Höck</a></p>
--			<p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
--		</footer>
--		<!-- Scripts here. Don't remove ↓ -->
--		<script src="node_modules/todomvc-common/base.js"></script>
--		<script src="js/app.js"></script>
--	</body>
