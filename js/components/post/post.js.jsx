import 'core-js/stable';
import 'regenerator-runtime/runtime';

import {SpecialistPost, MajorPost, MinorPost} from './post_components.js.jsx';

import React from 'react';
import ReactDOM from 'react-dom';


export default class CheckMyPost extends React.Component {
    constructor(props) {
        super(props);

        this.changeActiveTab = this.changeActiveTab.bind(this);
        this.updateNavCreditCounts = this.updateNavCreditCounts.bind(this);
        this.updatePostStatus = this.updatePostStatus.bind(this);
    }

    componentDidMount() {
        var activeTab = this.refs.postNav.state.activeTab + 'Post';
        this.changeActiveTab(activeTab);
        this.updateNavCreditCounts();
    }

    changeActiveTab(newTab) {
        var activeTab = newTab;
        var tabs = ['spePost', 'majPost', 'minPost'];

        tabs.forEach((tab) => {
            if (tab === activeTab) {
                this.refs[tab].changeTabView(true);
            } else {
                this.refs[tab].changeTabView(false);
            }
        });
    }

    updateNavCreditCounts() {
        var newCounts = [this.refs.spePost.getCreditCount(),
                         this.refs.majPost.getCreditCount(),
                         this.refs.minPost.getCreditCount()];
        this.refs.postNav.setState({creditCounts: newCounts},
            this.updatePostStatus());
    }

    updatePostStatus() {
        var newStatuses = [this.refs.spePost.setIfCompleted(),
                           this.refs.majPost.setIfCompleted(),
                           this.refs.minPost.setIfCompleted()];
        this.refs.postNav.setState({completed: newStatuses});
    }

    render() {
        return (
            <div id='check_my_post'>
                <PostNav ref='postNav'
                         updateTab={this.changeActiveTab}
                         getCreditCountClass={this.getCreditCountClass} />
                <SpecialistPost ref='spePost' />
                <MajorPost ref='majPost' />
                <MinorPost ref='minPost' />
            </div>
        );
    }
}


class PostNav extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            visible: localStorage.getItem('activePost') === '' ? 'spe' : localStorage.getItem('activePost'),
            creditCounts: [0.0, 0.0, 0.0],
            completed: [false, false, false],
            activeTab: localStorage.getItem('activePost')
        };

        this.getActiveTab = this.getActiveTab.bind(this);
        this.changeActiveTab = this.changeActiveTab.bind(this);
        this.getNavClass = this.getNavClass.bind(this);
        this.getCreditCountClass = this.getCreditCountClass.bind(this);
    }

    getActiveTab() {
        return localStorage.getItem('activePost');
    }

    changeActiveTab(e) {
        var newVisible = e.target.id.substring(0, 3);
        this.setState({visible: newVisible}, () => {
            this.props.updateTab(newVisible + 'Post');
            localStorage.setItem('activePost', newVisible);
        });
    }

    getNavClass(type) {
        return this.state.visible === type ? 'nav_selected' : 'nav_not_selected';
    }

    getCreditCountClass(postIndex) {
        return this.state.completed[postIndex] ? 'credits_completed' : 'credits_not_completed';
    }

    render() {
        return (
            <nav id='posts'>
                <ul>
                    <li id='specialist' className={this.getNavClass('spe')}>
                        <a id='spec_link' onClick={this.changeActiveTab}> Specialist </a>
                        <div id='spec_creds' className={this.getCreditCountClass(0)}>
                            {'(' + this.state.creditCounts[0].toFixed(1) + '/12.0)'} </div>
                    </li>
                    <li id='major' className={this.getNavClass('maj')}>
                        <a id='maj_link' onClick={this.changeActiveTab}> Major </a>
                        <div id='maj_creds' className={this.getCreditCountClass(1)}>
                            {'(' + this.state.creditCounts[1].toFixed(1) + '/8.0)'} </div>
                    </li>
                    <li id='minor' className={this.getNavClass('min')}>
                        <a id='min_link' onClick={this.changeActiveTab}> Minor </a>
                        <div id='min_creds' className={this.getCreditCountClass(2)}>
                        {'(' + this.state.creditCounts[2].toFixed(1) + '/4.0)'} </div>
                    </li>
                </ul>
            </nav>
        );
    }

}


ReactDOM.render(
    <CheckMyPost />,
    document.getElementById('all_posts')
);
